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

exception DbfDataInvalid of string

val string_of_zstring : string -> string

val dbf_open : string -> dbf_file
val dbf_close : dbf_file -> unit
val db_goto : dbf_file -> int -> dbf_file
val db_skip : dbf_file -> int -> dbf_file
val db_go_top : dbf_file -> dbf_file
val db_go_bottom : dbf_file -> dbf_file
val db_bof : dbf_file -> bool

val read_raw_record : dbf_file -> string
val read_record :
  ?with_deleted:bool -> dbf_file -> Bitstring.bitstring option
val is_deleted : Bitstring.bitstring option -> bool

val db_next : dbf_file -> dbf_file
val db_struct : dbf_file -> dbf_field_descriptor array
val db_lastrec : dbf_file -> int
val db_find_record :
  dbf_file -> (Bitstring.bitstring option -> bool) -> int option
val db_find_record_simple :
  dbf_file -> (Bitstring.bitstring option -> 'a) -> 'a -> int option
val dbfile_format_of_byte : int -> dbfile_format

val dbf_has_memo_fields : dbf_file -> bool