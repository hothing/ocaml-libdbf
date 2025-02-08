type db_value =                 
    DbString of string
  | DbNumber of int64
  | DbLogical of bool
  | DbMemoIndex of int
  | DbDate of int64
  | DbFloat of float
  | DbTimestamp of int64
val bs_to_int : int -> int -> bytes * int * int -> int
val bs_to_bstr : int -> int -> bytes * int * int -> string
val bs_to_str : int -> int -> bytes * int * int -> string
val bs_to_int64 : int -> int -> bytes * int * int -> int64
val bs_to_float64 : int -> int -> bytes * int * int -> float
val bs_to_logical : int -> int -> bytes * int * int -> bool
val ftype_to_string : Dbf.Base.dbf_data_type -> string
val get_field : ('a -> 'b) -> 'a option -> 'b
val create_int_rider :
  Dbf.Base.dbf_file -> int -> (bytes * int * int) option -> int
val create_int64_rider :
  Dbf.Base.dbf_file -> int -> (bytes * int * int) option -> int64
val create_bstring_rider :
  Dbf.Base.dbf_file -> int -> (bytes * int * int) option -> string
val create_string_rider :
  Dbf.Base.dbf_file -> int -> (bytes * int * int) option -> string
val create_bool_rider :
  Dbf.Base.dbf_file -> int -> (bytes * int * int) option -> bool
val create_float_rider :
  Dbf.Base.dbf_file -> int -> (bytes * int * int) option -> float
val create_int_rider_by_name :
  Dbf.Base.dbf_file -> string -> (bytes * int * int) option -> int
val create_int64_rider_by_name :
  Dbf.Base.dbf_file -> string -> (bytes * int * int) option -> int64
val create_float64_rider_by_name :
  Dbf.Base.dbf_file -> string -> (bytes * int * int) option -> float
val create_string_rider_by_name :
  Dbf.Base.dbf_file -> string -> (bytes * int * int) option -> string
val create_bstring_rider_by_name :
  Dbf.Base.dbf_file -> string -> (bytes * int * int) option -> string
val create_bool_rider_by_name :
  Dbf.Base.dbf_file -> string -> (bytes * int * int) option -> bool
val db_value_type_str : db_value -> string
val db_value_to_string : db_value -> string
