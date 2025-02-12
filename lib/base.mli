(** Database file descriptor *)  
type dbf_file = {
  name : string; (** name of dBASE table or filename *)
  fin : in_channel; (** opened database file channel *)
  memo : in_channel option; (** opened database memo file channel *)
  info : dbf_info; (** description of table structure  *)
  cri : int; (** current record index *)
}

(** Type [dbf_info] describes the DBF file structure  *)
and dbf_info = {               
  version : dbfile_format; (** version/format of the database *)
  mdate : date; (** modification date *)
  num_records : int; (** number of records (include deleted) *)
  hdr_size : int; (** header size (in bytes) of the database file *)
  rec_size : int; (** the record size in bytes *)
  fields : dbf_field_descriptor array; (** array of the field descriptors *)
}

(** Type [date] describes date in format (year , month , day) *)
and date = { year: int ; month: int; day: int}

(** Database filed descriptor *)
and dbf_field_descriptor = {
  name : string; (** name of field *)
  ftype : dbf_data_type; (** data type of field *)
  faddr : int; (** offset of the field in a memory chunk *)
  flen : int; (** length of the field in bytes *)
  fdec : int; (** position of decimal point *)
  work_area_id : int; (** work ared identity - IT IS NOT USED *)
  flags : int; (** internally used flags *)
}

(** *)
and dbf_data_type =
    Character (** one char or ASCII-string up to 255 character *)
  | Number (** represents any number: integer or real *)
  | Float (** represents floating-point number *)
  | Date (** represents DATE in format YYYYMMDD *)
  | Logical (** logical (boolean) value *)
  | Memo (** reference to the MEMO file record/chunk *)
  | General of int (** general purpose *)


(** *)
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

  val db_has_memo_fields : dbf_file -> bool