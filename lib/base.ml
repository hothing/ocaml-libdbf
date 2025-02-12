type dbf_file = {
  name : string;
  fin  : in_channel;
  memo : in_channel option;
  info : dbf_info;
  cri  : int; (* current record index *)
}

and dbf_info = {
  version : dbfile_format;
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

and dbfile_format =
  | DBASE2
  | DBASE3_no_memo
  | DBASE3_with_memo
  | VisualFoxPro
  | VisualFoxPro_autoincrement
  | VisualFoxPro_Varbinary
  | DBASE4_with_memo
  | DBASE4_SQL_table_files_no_memo
  | DBASE4_SQL_table_files_with_memo
  | DBASE4_SQL_system_files_no_memo

module Header = struct
  [%%cstruct
  type t = {
    version : uint8_t;
    mdate_year : uint8_t;
    mdate_month : uint8_t;
    mdate_day : uint8_t;
    num_records : uint32_t;
    hdr_size : uint16_t;
    rec_size : uint16_t;
  }
  [@@little_endian]]
end

module Field = struct
  [%%cstruct
  type t = {
    name : uint8_t; [@len 11]
    datatype : char;
    reserved0 : uint32_t;
    length : uint8_t;
    decimal_count : uint8_t;
    reserved1 : uint16_t;
    work_area_id : uint8_t;
    reserved2 : uint16_t;
    field_flags : uint8_t;
    reserved3 : uint8_t; [@len 8]
  }
  [@@little_endian]]
end

exception DbfDataInvalid of string

let dbfile_format_opt = function
  | 0x3 -> Some(DBASE3_no_memo)
  | 0x30 -> Some(VisualFoxPro)
  | 0x31 -> Some(VisualFoxPro_autoincrement)
  | 0x32 -> Some(VisualFoxPro_Varbinary)
  | 0x43 -> Some(DBASE4_SQL_table_files_no_memo)
  | 0x63 -> Some(DBASE4_SQL_system_files_no_memo)
  | 0x83 -> Some(DBASE3_with_memo)
  | 0x8B -> Some(DBASE4_with_memo)
  | 0xCB -> Some(DBASE4_SQL_table_files_with_memo)
  | _ -> None

let decode_hdr0 cst =
  match dbfile_format_opt (Header.get_t_version cst) with
  | Some ver -> {
      version = ver;
      mdate =
        { year = 1900 + (Header.get_t_mdate_year cst);
          month = (Header.get_t_mdate_month cst);
          day = (Header.get_t_mdate_day cst )
        };
      num_records = Int32.to_int (Header.get_t_num_records cst);
      hdr_size = Header.get_t_hdr_size cst;
      rec_size = Header.get_t_rec_size cst;
      fields = [||];
    }
  | None -> raise (DbfDataInvalid "DBF format is not V3+")

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

let decode_field x =
  let name =
    Field.get_t_name x |> Cstruct.to_string |> string_of_zstring |> fixup_str
  in
  let ftype = dbf_data_type (Field.get_t_datatype x) in
  {
    name;
    ftype;
    flen = Field.get_t_length x;
    fdec = Field.get_t_decimal_count x;
    work_area_id = Field.get_t_work_area_id x;
    flags = Field.get_t_field_flags x;
    faddr = 0;
  }

let calculate_fields_offset (flds : dbf_field_descriptor list) =
  let clc_offset offset (fd : dbf_field_descriptor) =
    (offset + fd.flen, { fd with faddr = offset })
  in
  let _, fol = List.fold_left_map clc_offset 0 flds in
  fol

let decode_struct hdr_size t =
  let rec loop acc pos =
    if pos < hdr_size - Field.sizeof_t then
      match Cstruct.get_uint8 t pos with
      | 0x0D -> List.rev acc
      | _ ->
          let chunk = Cstruct.sub t pos Field.sizeof_t in
          let f = decode_field chunk in
          loop (f :: acc) (pos + Field.sizeof_t)
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
  let open Bigarray in
  let buf = Array1.init Char c_layout size (fun _ -> Char.chr 0x0) in
  let _ = In_channel.input_bigarray fin buf 0 size in
  Cstruct.of_bigarray buf

let dbf_open filename =
  let fin = In_channel.open_bin filename in
  let hsz = 0x20 in
  let hdr0 = read_chunk fin hsz |> decode_hdr0 in
  let fsz = hdr0.hdr_size - hsz in
  let _ = In_channel.seek fin (Int64.of_int hsz) in
  let fields = read_chunk fin fsz |> decode_struct fsz |> Array.of_list in
  let _ = In_channel.seek fin (Int64.of_int hdr0.hdr_size) in
  let moin = (match (db_memo_exists filename) with
      | Some mn -> Some(In_channel.open_bin mn)
      | None -> None)
  in   
  { name = filename; fin; memo = moin; info = { hdr0 with fields }; cri = 0; }

let dbf_close dbf =
  In_channel.close dbf.fin;
  match dbf.memo with Some mi -> In_channel.close mi | None -> ()

let get_record_offset (hdr : dbf_info) index =
  let idx = if index > 0 then index else 1 in
  hdr.hdr_size + (hdr.rec_size * (idx - 1))

let read_raw_record (db : dbf_file) =
  read_chunk db.fin db.info.rec_size |> Cstruct.to_string

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

let is_deleted (rbs : Bitstring.bitstring option) =
  match rbs with
  | Some _ -> (* valid record *) false
  | None -> (* deleted record *) true

let read_record ?(with_deleted = false) db =
  let db = db_skip db 0 in (* syncronize record position and OS file handle position *)
  let chunk = read_chunk db.fin db.info.rec_size in
  let data = Cstruct.sub chunk 1 (db.info.rec_size - 1) in
  let bsd = data |> Cstruct.to_string |> Bitstring.bitstring_of_string in
  let ch = Char.chr (Cstruct.get_uint8 chunk 0) in
  match ch with
  | ' ' -> (* valid record *) Some bsd
  | _ -> (* deleted record *) 
    if not with_deleted then None else Some bsd

let db_next dbf =
  let dbf = ref (db_skip dbf 1) in
  let drec = read_record !dbf in
  while not (db_bof !dbf) && (is_deleted drec) do dbf := (db_skip !dbf 1) done;
  !dbf

(* Create an array containing the structure of a database file *)
let db_struct dbf = dbf.info.fields

let db_lastrec dbf = dbf.info.num_records

let db_find_record dbf pred =
  let rec _find_record dbf pred =
    if not (db_bof dbf) then
      let drec = read_record dbf in 
      if (is_deleted drec) then 
        _find_record (db_skip dbf 1) pred
      else 
        (if pred drec then Some dbf.cri else 
          _find_record (db_skip dbf 1) pred)
    else None
  in
  (* let dbf = db_go_top dbf in *)
  _find_record dbf pred

let db_find_record_simple dbf field_rider value =
  let pred drec = (field_rider drec) = value in
  db_find_record dbf pred

