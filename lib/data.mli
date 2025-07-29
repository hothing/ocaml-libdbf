(* Data.ml *)
open Base

val create_int_rider :
  dbf_file -> int -> bytes -> int
val create_int64_rider :
  dbf_file -> int -> bytes -> int64
val create_bstring_rider :
  dbf_file -> int -> bytes -> string
val create_string_rider :
  dbf_file -> int -> bytes -> string
val create_bool_rider :
  dbf_file -> int -> bytes -> bool
val create_float_rider :
  dbf_file -> int -> bytes -> float
val create_int_rider_by_name :
  dbf_file -> string -> bytes -> int
val create_int64_rider_by_name :
  dbf_file -> string -> bytes -> int64
val create_float64_rider_by_name :
  dbf_file -> string -> bytes -> float
val create_string_rider_by_name :
  dbf_file -> string -> bytes -> string
val create_bstring_rider_by_name :
  dbf_file -> string -> bytes -> string
val create_bool_rider_by_name :
  dbf_file -> string -> bytes -> bool
