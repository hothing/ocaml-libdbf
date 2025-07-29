(******* Memo File support ********)
let memo_block_size = 512

let calculate_chunk_offset memo_block_size index = index * memo_block_size

(******* dBase III         ********)

let bytes_rscan_eob bs =
  let n = Bytes.length bs in
  let rec _rscan_eob bs p =
    if p > 0 then
      if (Bytes.get_int16_le bs p) = 0x1A1A then p 
      else _rscan_eob bs (p - 1)
    else -1
  in  
  _rscan_eob bs n

let rec read_cblock_chn n chn pos data =
  let rlen = In_channel.input chn data pos n in
  if rlen >= 0 then
    let p = bytes_rscan_eob data in
    if p >= 0 then Bytes.sub data 0 p
    else read_cblock_chn n chn (pos + rlen) data
  else data

let read_block_db3 (db : Base.dbf_file) index =
  match db.memo with
  | Some md ->
      let pos = calculate_chunk_offset memo_block_size index in
      In_channel.seek md (Int64.of_int pos);
      let data =
        read_cblock_chn memo_block_size md 0
          (Bytes.make (memo_block_size * 4) '\026')
      in
      Some (data)
  | None -> None

(******* dBase IV          ********)

let read_block_db4 (db : Base.dbf_file) index =
  match db.memo with
  | Some md ->
      if index > 0 then (
        let pos = calculate_chunk_offset memo_block_size index in
        In_channel.seek md (Int64.of_int pos);
        let hdr_size = 8 in
        let chunk = Base.read_chunk md hdr_size in
        let _mark = Bytes.get_int32_le chunk 0 |> Int32.to_int in
        let _len = Int32.to_int (Bytes.get_int32_le chunk 4) in
        (*assert(_mark = 589823, _msg); *)
        let _mark_check = 589823 in
        if _mark = _mark_check then
          (
            In_channel.seek md (Int64.of_int (pos + hdr_size));
            let _data = Base.read_chunk md (_len - hdr_size) in
            Some _data
          )
        else 
          ( 
            Printf.eprintf 
            "[MEMO.ERROR]: _mark expected 0x%x, got 0x%x at pos = 0x%x\n" 
            _mark_check _mark pos;          
            None 
          );
      )
      else None
  | None -> None

(* --------- *)
let load_memo_internal rider (db : Base.dbf_file) index =
  let memo_to_dbval (obs : Bytes.t option) =
    match obs with
    | Some data -> data
    | None -> Bytes.empty
  in
  memo_to_dbval (rider db index)

let load_memo (db : Base.dbf_file) index =
  match db.info.version with
  | 0x83 ->
      load_memo_internal read_block_db3 db index
  | 0x8b -> 
      load_memo_internal read_block_db4 db index
  | _ -> failwith "This dBase version is not supported"