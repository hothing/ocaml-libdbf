open Base

(* low-level field rider/getter*)

let bs_to_int offset len bs =
  assert (len <= 18);
  match%bitstring bs with
  | {| _data:(len * 8):string,offset(offset*8) |} -> (
      try String.trim _data |> String.cat "0" |> int_of_string
      with _ ->
        raise
          (DbfDataInvalid (Printf.sprintf "Wrong memo index, value = %s" _data))
      )
  | {| _ |} -> raise (DbfDataInvalid "field: cannot extract value")

(* *)
let bs_to_bstr offset len bs =
  assert (len <= 254);
  match%bitstring bs with
  | {| _data:(len * 8):string,offset(offset*8) |} -> _data
  | {| _ |} -> raise (DbfDataInvalid "field: cannot extract value")

let bs_to_str offset len bs =
  assert (len <= 255);
  match%bitstring bs with
  | {| _data:(len * 8):string,offset(offset*8) |} ->
      string_of_zstring _data |> String.trim
  | {| _ |} -> raise (DbfDataInvalid "field: cannot extract value")

let bs_to_int64 offset len bs =
  assert (len <= 18);
  let ns = bs_to_str offset len bs |> String.trim in
  let ns' =
    if String.length ns > 0 then ns else "0" (* field is uninitialized *)
  in
  try Int64.of_string ns'
  with _ ->
    raise
      (DbfDataInvalid
         (Printf.sprintf
            "Wrong number representation in DBF record field, value = %s" ns))

let bs_to_float64 offset len bs =
  assert (len <= 18);
  let ns = bs_to_str offset len bs |> String.trim in
  let ns' =
    if String.length ns > 0 then ns else "0.0" (* field is uninitialized *)
  in
  try Float.of_string ns'
  with _ ->
    raise
      (DbfDataInvalid
         (Printf.sprintf
            "Wrong number representation in DBF record field, value = %s" ns))

let bs_to_logical offset len bs =
  assert (len = 1);
  match%bitstring bs with
  | {| _data:8:offset(offset*8) |} -> (
      match Char.chr _data with
      | ' ' | '?' -> (* uninitialized *) false
      | 'Y' | 'y' | 'T' | 't' -> true
      | 'N' | 'n' | 'F' | 'f' -> false
      | '\001' -> true (* fuck you, Siemens IAD *)
      | '\000' -> false (* fuck you, Siemens IAD *)
      | x ->
          raise
            (DbfDataInvalid
               (Printf.sprintf
                  "Wrong number representation in DBF record field, value = %c"
                  x)))
  | {| _ |} -> raise (DbfDataInvalid "field: cannot extract value")
(* *)

let ftype_to_string ft =
  match ft with
  | Character -> "CHAR"
  | Number -> "NUMBER"
  | Logical -> "LOGICAL"
  | Memo -> "MEMO"
  | Date -> "DATE"
  | Float -> "FLOAT"
  | General id -> Printf.sprintf "GENERAL(%d)" id

let get_field rider rbs =
  match rbs with
  | Some data -> (* valid record *) rider data
  | None ->
      (* deleted record *) raise (DbfDataInvalid "Current record is deleted")

let create_int_rider (db : dbf_file) field_index =
  let field = db.info.fields.(field_index) in
  match field.ftype with
  | Number when field.fdec = 0 -> get_field (bs_to_int field.faddr field.flen)
  | Memo -> get_field (bs_to_int field.faddr field.flen)
  | _ -> raise (DbfDataInvalid "Field is not INTEGER")

let create_int64_rider (db : dbf_file) field_index =
  let field = db.info.fields.(field_index) in
  match field.ftype with
  | Number when field.fdec = 0 -> get_field (bs_to_int64 field.faddr field.flen)
  | _ -> raise (DbfDataInvalid "Field is not INTEGER_64")

let create_bstring_rider (db : dbf_file) field_index =
  let field = db.info.fields.(field_index) in
  match field.ftype with
  | Character -> get_field (bs_to_bstr field.faddr field.flen)
  | _ -> raise (DbfDataInvalid "Field is not CHAR(n)")

let create_string_rider (db : dbf_file) field_index =
  let field = db.info.fields.(field_index) in
  match field.ftype with
  | Character -> get_field (bs_to_str field.faddr field.flen)
  | _ -> raise (DbfDataInvalid "Field is not CHAR(n)")

let create_bool_rider (db : dbf_file) field_index =
  let field = db.info.fields.(field_index) in
  match field.ftype with
  | Logical -> get_field (bs_to_logical field.faddr field.flen)
  | _ -> raise (DbfDataInvalid "Field is not LOGICAl/BOOLEAN")

let create_float_rider (db : dbf_file) field_index =
  let field = db.info.fields.(field_index) in
  match field.ftype with
  | Number when field.fdec > 0 ->
      get_field (bs_to_float64 field.faddr field.flen)
  | Float -> get_field (bs_to_float64 field.faddr field.flen)
  | _ -> raise (DbfDataInvalid "Field is not FLOAT")

let create_int_rider_by_name (db : dbf_file) fieldname =
  let fid =
    Array.find_index
      (fun (f : dbf_field_descriptor) -> f.name = fieldname)
      db.info.fields
  in
  match fid with
  | Some id -> create_int_rider db id
  | None -> raise (DbfDataInvalid "Fieldname not found")

let create_int64_rider_by_name (db : dbf_file) fieldname =
  let fid =
    Array.find_index
      (fun (f : dbf_field_descriptor) -> f.name = fieldname)
      db.info.fields
  in
  match fid with
  | Some id -> create_int64_rider db id
  | None -> raise (DbfDataInvalid "Fieldname not found")

let create_float64_rider_by_name (db : dbf_file) fieldname =
  let fid =
    Array.find_index
      (fun (f : dbf_field_descriptor) -> f.name = fieldname)
      db.info.fields
  in
  match fid with
  | Some id -> create_float_rider db id
  | None -> raise (DbfDataInvalid "Fieldname not found")

let create_string_rider_by_name (db : dbf_file) fieldname =
  let fid =
    Array.find_index
      (fun (f : dbf_field_descriptor) -> f.name = fieldname)
      db.info.fields
  in
  match fid with
  | Some id -> create_string_rider db id
  | None -> raise (DbfDataInvalid "Fieldname not found")

let create_bstring_rider_by_name (db : dbf_file) fieldname =
  let fid =
    Array.find_index
      (fun (f : dbf_field_descriptor) -> f.name = fieldname)
      db.info.fields
  in
  match fid with
  | Some id -> create_bstring_rider db id
  | None -> raise (DbfDataInvalid "Fieldname not found")

let create_bool_rider_by_name (db : dbf_file) fieldname =
  let fid =
    Array.find_index
      (fun (f : dbf_field_descriptor) -> f.name = fieldname)
      db.info.fields
  in
  match fid with
  | Some id -> create_bool_rider db id
  | None -> raise (DbfDataInvalid "Fieldname not found")
    
