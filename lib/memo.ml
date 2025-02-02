(******* Memo File support ********)
let memo_block_size = 512

type mem_block_type =
  | Unused_Block of int * int
  | Used_Block of Bitstring.bitstring

let calculate_chunk_offset memo_block_size index = index * memo_block_size

(******* dBase III         ********)
let check_rep_char m n st c =
  let pos, cnt = st in
  if c = m then (pos, cnt + 1) else if cnt < n then (pos + 1, 0) else st

let check_rep_char2 m n c st =
  let pos, cnt = st in
  if c = m then (pos, cnt + 1) else if cnt < n then (pos + 1, 0) else st

let check_rep_char_rf m n c st =
  let pos, cnt = st in
  if c = m then (pos + 1, cnt + 1) else if cnt < n then (pos + 1, 0) else st

let scan_end_of_block str =
  let pos, c = String.fold_left (check_rep_char '\026' 2) (0, 0) str in
  if c >= 2 then pos + 1 else 0

let str_rscan_rep ch n str =
  let pos, c = String.fold_right (check_rep_char_rf ch n) str (0, 0) in
  if c >= n then (String.length str - pos, true) else (0, false)

let bytes_rscan_rep ch n bs =
  let pos, c = Bytes.fold_right (check_rep_char_rf ch n) bs (0, 0) in
  if c >= n then (Bytes.length bs - pos, true) else (0, false)

let rec read_cblock_str n data pos src =
  if String.length src >= pos + n then
    let ss = String.sub src pos n in
    let data' = data ^ ss in
    let slen, ok = str_rscan_rep '\026' 2 data' in
    if slen > 0 && ok then String.sub data' 0 slen
    else if ok then data
    else read_cblock_str n data' (pos + n) src
  else data

let rec read_cblock_chn n chn pos data =
  let rlen = In_channel.input chn data pos n in
  if rlen >= 0 then
    let slen, ok = bytes_rscan_rep '\026' 2 data in
    if slen > 0 && ok then Bytes.sub_string data 0 slen
    else if ok then Bytes.to_string data
    else read_cblock_chn n chn (pos + rlen) data
  else Bytes.to_string data

let read_block_db3 (db : Base.dbf_file) index =
  match db.memo with
  | Some md ->
      let pos = calculate_chunk_offset memo_block_size index in
      In_channel.seek md (Int64.of_int pos);
      let data =
        read_cblock_chn memo_block_size md 0
          (Bytes.make (memo_block_size * 4) '\026')
      in
      Some (Bitstring.bitstring_of_string data)
  | None -> None

(******* dBase IV          ********)

let read_block_db4 (db : Base.dbf_file) index =
  match db.memo with
  | Some md ->
      if index > 0 then (
        let pos = calculate_chunk_offset memo_block_size index in
        In_channel.seek md (Int64.of_int pos);
        let hdr_size = 8 in
        let chunk = Bitstring.bitstring_of_chan_max md hdr_size in
        match%bitstring chunk with
        | {| _mark:32:littleendian,check(_mark = Int32.of_int 589823); _len:32:littleendian,bind(Int32.to_int _len)|}
          ->
            In_channel.seek md (Int64.of_int (pos + hdr_size));
            let _data = Bitstring.bitstring_of_chan_max md (_len - hdr_size) in
            Some _data
        | {| _next_free:32:littleendian,bind(Int32.to_int _next_free); _next_used:32:littleendian,bind(Int32.to_int _next_used)|}
          ->
            None
        | {| _ |} -> failwith "Wrong Memo chunk")
      else None
  | None -> None

let read_block_db4_ext (db : Base.dbf_file) index =
  match db.memo with
  | Some md -> (
      let pos = calculate_chunk_offset memo_block_size index in
      In_channel.seek md (Int64.of_int pos);
      let hdr_size = 8 in
      let chunk = Bitstring.bitstring_of_chan_max md hdr_size in
      match%bitstring chunk with
      | {| _mark:32:littleendian,check(_mark = Int32.of_int 589823); _len:32:littleendian,bind(Int32.to_int _len)|}
        ->
          In_channel.seek md (Int64.of_int (pos + hdr_size));
          let _data = Bitstring.bitstring_of_chan_max md (_len - hdr_size) in
          Some (Used_Block _data)
      | {| _next_free:32:littleendian,bind(Int32.to_int _next_free); _next_used:32:littleendian,bind(Int32.to_int _next_used)|}
        ->
          Some (Unused_Block (_next_free, _next_used))
      | {| _ |} -> failwith "Wrong Memo chunk")
  | None -> None

(* --------- *)
let load_memo_internal rider (db : Base.dbf_file) index =
  let memo_to_dbval (obs : Bitstring.t option) =
    match obs with
    | Some data -> Bitstring.string_of_bitstring data
    | None -> ""
  in
  memo_to_dbval (rider db index)

let load_memo (db : Base.dbf_file) index =
  match Base.dbfile_format_of_byte db.info.version with
  | FoxBASE_plus_dBASE_III_PLUS_with_memo ->
      load_memo_internal read_block_db3 db index
  | DBASE_IV_with_memo -> 
      load_memo_internal read_block_db4 db index
  | _ -> ""
