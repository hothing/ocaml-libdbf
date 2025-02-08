(* Memo.ml *)
open Base

val memo_block_size : int       
type mem_block_type =
    Unused_Block of int * int
  | Used_Block of Bitstring.bitstring
val calculate_chunk_offset : int -> int -> int
val bytes_rscan_eob : bytes -> int
val read_cblock_str : int -> bytes -> int -> bytes -> bytes
val read_cblock_chn : int -> In_channel.t -> int -> bytes -> bytes
val read_block_db3 : dbf_file -> int -> Bitstring.bitstring option
val read_block_db4 : dbf_file -> int -> Bitstring.bitstring option
val read_block_db4_ext : dbf_file -> int -> mem_block_type option
val load_memo_internal :
  (dbf_file -> 'a -> Bitstring.t option) ->
  dbf_file -> 'a -> string
val load_memo : dbf_file -> int -> string