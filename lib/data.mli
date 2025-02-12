(* Data.ml *)
open Base

val ftype_to_string : dbf_data_type -> string
val get_field : ('a -> 'b) -> 'a option -> 'b
val create_int_rider :
  dbf_file -> int -> (bytes * int * int) option -> int
val create_int64_rider :
  dbf_file -> int -> (bytes * int * int) option -> int64
val create_bstring_rider :
  dbf_file -> int -> (bytes * int * int) option -> string
val create_string_rider :
  dbf_file -> int -> (bytes * int * int) option -> string
val create_bool_rider :
  dbf_file -> int -> (bytes * int * int) option -> bool
val create_float_rider :
  dbf_file -> int -> (bytes * int * int) option -> float
val create_int_rider_by_name :
  dbf_file -> string -> (bytes * int * int) option -> int
val create_int64_rider_by_name :
  dbf_file -> string -> (bytes * int * int) option -> int64
val create_float64_rider_by_name :
  dbf_file -> string -> (bytes * int * int) option -> float
val create_string_rider_by_name :
  dbf_file -> string -> (bytes * int * int) option -> string
val create_bstring_rider_by_name :
  dbf_file -> string -> (bytes * int * int) option -> string
val create_bool_rider_by_name :
  dbf_file -> string -> (bytes * int * int) option -> bool
