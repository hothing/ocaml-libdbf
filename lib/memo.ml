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
      Some (Bytes.to_string data |> Bitstring.bitstring_of_string)
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

(* --------- *)
let load_memo_internal rider (db : Base.dbf_file) index =
  let memo_to_dbval (obs : Bitstring.t option) =
    match obs with
    | Some data -> Bitstring.string_of_bitstring data
    | None -> ""
  in
  memo_to_dbval (rider db index)

let load_memo (db : Base.dbf_file) index =
  match db.info.version with
  | DBASE3_with_memo ->
      load_memo_internal read_block_db3 db index
  | DBASE4_with_memo -> 
      load_memo_internal read_block_db4 db index
  | _ -> ""
