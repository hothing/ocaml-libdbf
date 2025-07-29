type dbf_file = {
  name : string;
  fin  : in_channel;
  memo : in_channel option;
  info : dbf_info;
  cri  : int; (* current record index *)
}

and dbf_info = {
  version : int;
  mdate : date;
  num_records : int;
  hdr_size : int;
  rec_size : int;
  fields : dbf_field_descriptor array;
}

and date = { year: int ; month: int; day: int}

and dbf_field_descriptor = {
  name : string;
  ftype : dbf_data_type;
  faddr : int; (* replacement: calculated offset in the chunk *)
  flen : int; (* byte length of field *)
  fdec : int;
  work_area_id : int;
  flags : int;
}

and dbf_data_type =
  | Character
  | Number
  | Float
  | Date
  | Logical
  | Memo
  | General of int

exception DbfDataInvalid of string

(*
type t = {
    version : uint8_t;
    mdate_year : uint8_t;
    mdate_month : uint8_t;
    mdate_day : uint8_t;
    num_records : uint32_t;
    hdr_size : uint16_t;
    rec_size : uint16_t;
  }
    size = 3*4 -> 12
*)
let decode_hdr0 bstr =
  let version = Bytes.get_uint8 bstr 0 in
  let _mdate_year = Bytes.get_uint8 bstr 1 in
  let _mdate_month = Bytes.get_uint8 bstr 2 in
  let _mdate_day = Bytes.get_uint8 bstr 3 in
  let num_records = Int32.to_int (Bytes.get_int32_le bstr 4) in
  let hdr_size = Bytes.get_uint16_le bstr 8 in
  let rec_size = Bytes.get_uint16_le bstr 10 in
  let mdate = { year = 1900 + _mdate_year; month = _mdate_month; day = _mdate_day} in
  {
    version;
    mdate;
    num_records;
    hdr_size;
    rec_size;
    fields = [||];
  }
  

let fixup_str bstr =
  let s = String.map (fun c -> if c <= '\t' then ' ' else c) bstr in
  String.trim s

let string_of_zstring s =
  match String.index_opt s '\x00' with
  | Some len -> String.sub s 0 len
  | None -> s

let dbf_data_type code =
  match code with
  | 'C' -> Character
  | 'L' -> Logical
  | 'M' -> Memo
  | 'N' -> Number
  | 'F' -> Float
  | 'D' -> Date
  | _ -> General (Char.code code)


let decode_field bstr offset =
  let name0 = Bytes.sub_string bstr offset 11 in
  let dt = Bytes.get bstr (offset + 0xb) in
  let flen = Bytes.get_uint8 bstr (offset + 0x10) in
  let fdec = Bytes.get_uint8 bstr (offset + 0x11) in
  let work_area_id = Bytes.get_uint8 bstr (offset + 0x14) in
  let flags = Bytes.get_uint8 bstr (offset + 0x17) in
  {
    name = fixup_str name0 |> string_of_zstring ;
    ftype = dbf_data_type dt;
    faddr = 0;
    flen;
    fdec;
    work_area_id;
    flags
  }

let calculate_fields_offset (flds : dbf_field_descriptor list) =
  let clc_offset offset (fd : dbf_field_descriptor) =
    (offset + fd.flen, { fd with faddr = offset })
  in
  let _, fol = List.fold_left_map clc_offset 0 flds in
  fol

let decode_struct hdr_size t =
  let fdsize = 0x20 in
  let rec loop acc pos =
    if pos < hdr_size - fdsize then
      match Bytes.get_uint8 t pos with
      | 0x0D -> List.rev acc
      | _ ->
          let f = decode_field t pos in
          loop (f :: acc) (pos + fdsize)
    else List.rev acc
  in
  loop [] 0 |> calculate_fields_offset

let db_has_memo_fields dbf =
  Array.exists (fun f -> match f.ftype with Memo -> true | _ -> false) dbf.info.fields  

let db_memo_exists filename =
  let bfn = Filename.chop_extension filename in
  let mfx = [ bfn ^ ".DBT"; bfn ^ ".dbt" ] in
  List.find_opt (fun fn -> Sys.file_exists fn) mfx

let read_chunk fin size =
  let buf = Bytes.make size ' ' in
  let _rl = In_channel.input fin buf 0 size in
  buf  

let dbf_open filename =
  let fin = In_channel.open_bin filename in
  let hsz = 0x20 in
  let hdr0 = read_chunk fin hsz |> decode_hdr0 in
  let fsz = hdr0.hdr_size - hsz in
  let _ = In_channel.seek fin (Int64.of_int hsz) in
  let fields = read_chunk fin fsz |> decode_struct fsz |> Array.of_list in
  let _ = In_channel.seek fin (Int64.of_int hdr0.hdr_size) in
  let memo = (match (db_memo_exists filename) with
      | Some mn -> Some(In_channel.open_bin mn)
      | None -> None)
  in   
  { name = filename; fin; memo; info = { hdr0 with fields }; cri = 0; }

let dbf_close dbf =
  In_channel.close dbf.fin;
  match dbf.memo with Some mi -> In_channel.close mi | None -> ()

let get_record_offset (hdr : dbf_info) index =
  let idx = if index > 0 then index else 1 in
  hdr.hdr_size + (hdr.rec_size * (idx - 1))

(* Position record pointer to a specific identity *)
let db_goto dbf index =
  if index <= dbf.info.num_records then
      let offset = get_record_offset dbf.info index in
      In_channel.seek dbf.fin (Int64.of_int offset) ;
      { dbf with cri = index; }
  else { dbf with cri = dbf.info.num_records + 1; }

(* Move relative to the current record *)
let db_skip dbf count =
  db_goto dbf (dbf.cri + count)
  
(* Move to the first logical record *)
let db_go_top (db : dbf_file) = db_goto db 1

(* Move to the last logical record *)
let db_go_bottom (db : dbf_file) = db_goto db db.info.num_records

let db_bof dbf = (dbf.cri > dbf.info.num_records) || (In_channel.pos dbf.fin >= In_channel.length dbf.fin)

let read_record ?(with_deleted = false) db =
  let chunk = read_chunk db.fin db.info.rec_size in
  let data = Bytes.sub chunk 1 ((Bytes.length chunk) - 1) in
  let ch = Char.chr (Bytes.get_uint8 chunk 0) in
  if (ch = ' ') || with_deleted then (false, data) else (true, Bytes.empty)
  
let iterator dbf =
  let dbf = db_go_top dbf in
  let next_record dbf =
    if not (db_bof dbf) then 
      let raw_rec = read_record dbf in
      let del, data = raw_rec in
      Some ((dbf.cri, del, data), (db_skip dbf 1))      
    else
      None
  in 
  Seq.unfold next_record dbf

let existed_rec dr = 
  let _, del, _ = dr in
  not del

let all_records dbf =
  let rseq = iterator dbf in
  let extractor dr =
    let _, _del, data = dr in
    if _del then Bytes.empty else data
  in
  Seq.filter existed_rec rseq 
  |> Seq.map extractor 