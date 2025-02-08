type dbf_info = {               
  version : int;
  mdate : int * int * int;
  num_records : int;
  hdr_size : int;
  rec_size : int;
  fields : dbf_field_descriptor array;
}
and date = int * int * int
and dbf_field_descriptor = {
  name : string;
  ftype : dbf_data_type;
  faddr : int;
  flen : int;
  fdec : int;
  work_area_id : int;
  flags : int;
}
and dbf_data_type =
    Character
  | Number
  | Float
  | Date
  | Logical
  | Memo
  | General of int
type dbf_file = {
  name : string;
  fin : in_channel;
  memo : in_channel option;
  info : dbf_info;
  cri : int;
}
type dbfile_format =
    FoxBASE
  | FoxBASE_plus_Dbase_III_plus_no_memo
  | Visual_FoxPro
  | Visual_FoxPro_autoincrement_enabled
  | Visual_FoxPro_with_field_type_Varchar_or_Varbinary
  | DBASE_IV_SQL_table_files_no_memo
  | DBASE_IV_SQL_system_files_no_memo
  | FoxBASE_plus_dBASE_III_PLUS_with_memo
  | DBASE_IV_with_memo
  | DBASE_IV_SQL_table_files_with_memo
  | FoxPro_2_x_or_earlier_with_memo
  | HiPer_Six_format_with_SMT_memo_file
module Header :
  sig
    val sizeof_t : int
    val get_t_version : Cstruct.t -> Cstruct.uint8
    val set_t_version : Cstruct.t -> Cstruct.uint8 -> unit
    val get_t_mdate_year : Cstruct.t -> Cstruct.uint8
    val set_t_mdate_year : Cstruct.t -> Cstruct.uint8 -> unit
    val get_t_mdate_month : Cstruct.t -> Cstruct.uint8
    val set_t_mdate_month : Cstruct.t -> Cstruct.uint8 -> unit
    val get_t_mdate_day : Cstruct.t -> Cstruct.uint8
    val set_t_mdate_day : Cstruct.t -> Cstruct.uint8 -> unit
    val get_t_num_records : Cstruct.t -> Cstruct.uint32
    val set_t_num_records : Cstruct.t -> Cstruct.uint32 -> unit
    val get_t_hdr_size : Cstruct.t -> Cstruct.uint16
    val set_t_hdr_size : Cstruct.t -> Cstruct.uint16 -> unit
    val get_t_rec_size : Cstruct.t -> Cstruct.uint16
    val set_t_rec_size : Cstruct.t -> Cstruct.uint16 -> unit
    val hexdump_t_to_buffer : Buffer.t -> Cstruct.t -> unit
    val hexdump_t : Cstruct.t -> unit
  end
module Field :
  sig
    val sizeof_t : int
    val get_t_name : Cstruct.t -> Cstruct.t
    val copy_t_name : Cstruct.t -> string
    val set_t_name : string -> int -> Cstruct.t -> unit
    val blit_t_name : Cstruct.t -> int -> Cstruct.t -> unit
    val get_t_datatype : Cstruct.t -> char
    val set_t_datatype : Cstruct.t -> char -> unit
    val get_t_reserved0 : Cstruct.t -> Cstruct.uint32
    val set_t_reserved0 : Cstruct.t -> Cstruct.uint32 -> unit
    val get_t_length : Cstruct.t -> Cstruct.uint8
    val set_t_length : Cstruct.t -> Cstruct.uint8 -> unit
    val get_t_decimal_count : Cstruct.t -> Cstruct.uint8
    val set_t_decimal_count : Cstruct.t -> Cstruct.uint8 -> unit
    val get_t_reserved1 : Cstruct.t -> Cstruct.uint16
    val set_t_reserved1 : Cstruct.t -> Cstruct.uint16 -> unit
    val get_t_work_area_id : Cstruct.t -> Cstruct.uint8
    val set_t_work_area_id : Cstruct.t -> Cstruct.uint8 -> unit
    val get_t_reserved2 : Cstruct.t -> Cstruct.uint16
    val set_t_reserved2 : Cstruct.t -> Cstruct.uint16 -> unit
    val get_t_field_flags : Cstruct.t -> Cstruct.uint8
    val set_t_field_flags : Cstruct.t -> Cstruct.uint8 -> unit
    val get_t_reserved3 : Cstruct.t -> Cstruct.t
    val copy_t_reserved3 : Cstruct.t -> string
    val set_t_reserved3 : string -> int -> Cstruct.t -> unit
    val blit_t_reserved3 : Cstruct.t -> int -> Cstruct.t -> unit
    val hexdump_t_to_buffer : Buffer.t -> Cstruct.t -> unit
    val hexdump_t : Cstruct.t -> unit
  end
exception DbfDataInvalid of string
val check_format_v3 : int -> bool
val decode_hdr0 : Cstruct.t -> dbf_info
val bit_test : int -> int -> bool
val fixup_str : string -> string
val string_of_zstring : string -> string
val dbf_data_type : char -> dbf_data_type
val decode_field : Cstruct.t -> dbf_field_descriptor
val calculate_fields_offset :
  dbf_field_descriptor list -> dbf_field_descriptor list
val decode_struct : int -> Cstruct.t -> dbf_field_descriptor list
val calculate_rec_size : dbf_field_descriptor array -> int
val dbf_has_memo : dbf_field_descriptor list -> bool
val dbf_memo_exists : string -> string option
val read_chunk : In_channel.t -> int -> Cstruct.t
val dbf_open : string -> dbf_file
val dbf_close : dbf_file -> unit
val get_record_offset : dbf_info -> int -> int
val read_raw_record : dbf_file -> string
val db_goto : dbf_file -> int -> dbf_file
val db_skip : dbf_file -> int -> dbf_file
val db_go_top : dbf_file -> dbf_file
val db_go_bottom : dbf_file -> dbf_file
val db_bof : dbf_file -> bool
val is_deleted : Bitstring.bitstring option -> bool
val read_record :
  ?with_deleted:bool -> dbf_file -> Bitstring.bitstring option
val db_next : dbf_file -> dbf_file
val db_struct : dbf_file -> dbf_field_descriptor array
val db_lastrec : dbf_file -> int
val db_find_record :
  dbf_file -> (Bitstring.bitstring option -> bool) -> int option
val db_find_record_simple :
  dbf_file -> (Bitstring.bitstring option -> 'a) -> 'a -> int option
val dbfile_format_of_byte : int -> dbfile_format

