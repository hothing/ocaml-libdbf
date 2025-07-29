(** DBF file handle *)
type dbf_file = {
  name : string; (** name of table (.dbf) *)
  fin : in_channel; (** opened i/o stream for dbf file *)
  memo : in_channel option; (** optional i/o stream for memo file - if exists *)
  info : dbf_info; (** description of DBF table *)
  cri : int; (** Current Rider Index - ID of current record in the table *)
}

(** descriptor of the internal dbf file layout *)
and dbf_info = {
  version : int; (** version of the file format *)
  mdate : date; (** modification date *)
  num_records : int; (** number of records *)
  hdr_size : int; (** file header size (in bytes) *)
  rec_size : int; (** file record size (in bytes) *)
  fields : dbf_field_descriptor array; (** fields descriptors *)
}

(** primitive structure to hold date *)
and date = { year : int; month : int; day : int; }

(** DBF field descriptor *) 
and dbf_field_descriptor = {
  name : string; (** name of field *)
  ftype : dbf_data_type; (** DBF data type *)
  faddr : int; (** address of field - it's using as offset in the raw record *)
  flen : int; (** length of field *)
  fdec : int; (** number after point for number data type *)
  work_area_id : int; (** reserved *)
  flags : int; (** some flags (not used) *)
}

(** internal DBF data type *)
and dbf_data_type =
    Character (** character (flen = 1) or string *)
  | Number (** number: integer (fdec = 0) or fixed point real *)
  | Float (** 32-bits floating point number *)
  | Date (** date *)
  | Logical (** boolean / logical value *)
  | Memo (** Text Large Object or Binary Large Object*)
  | General of int (** user-defined *)

(** Exception means: internal data representation is invalid *)
exception DbfDataInvalid of string

(** open DBF-file *)
val dbf_open : string -> dbf_file

(** close DBF-file *)
val dbf_close : dbf_file -> unit

(** move cursor to the n-th record, absolute *)
val db_goto : dbf_file -> int -> dbf_file

(** move cursor to the record after n-th records from the current, relative *)
val db_skip : dbf_file -> int -> dbf_file

(** move to the first record *)
val db_go_top : dbf_file -> dbf_file

(** move to the last record *)
val db_go_bottom : dbf_file -> dbf_file

(** check the end of file *)
val db_bof : dbf_file -> bool

(** check either has MEMO-extention or not *)
val db_has_memo_fields : dbf_file -> bool

val db_memo_exists : string -> string option

(** read raw (undecoded) record *)
val read_record : ?with_deleted:bool -> dbf_file -> bool * bytes

(** make an iterartor over all records *)
val iterator : dbf_file -> (int * bool * bytes) Seq.t

(** make an iterartor over all existed (not deleted) records *)
val all_records : dbf_file -> bytes Seq.t

(** FOR INTERNAL USE ONLY - translate binary ASCIIZ-string *)
val string_of_zstring : string -> string

(** FOR INTERNAL USE ONLY - read chunk of data from the file *)
val read_chunk : In_channel.t -> int -> bytes

