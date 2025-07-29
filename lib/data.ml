open Base

(* low-level field rider/getter*)

let bs_to_int offset len bs =
  assert (len <= 18);
  let _data = Bytes.sub_string bs offset len in
  try String.trim _data |> String.cat "0" |> int_of_string
  with _ ->
    (
      let msg = Printf.sprintf "Wrong integer repersenation, value = '%s' [%08d - %06d]" _data offset len in
      raise (DbfDataInvalid msg)
    )

let bs_to_bstr offset len bs =
  assert (len <= 255);
  Bytes.sub_string bs offset len

let bs_to_str offset len bs =
  assert (len <= 255);
  bs_to_bstr offset len bs |> Base.string_of_zstring |> String.trim

let bs_to_int64 offset len bs =
  assert (len <= 18);
  let ns = bs_to_str offset len bs in
  let ns' =
    if String.length ns > 0 then ns else "0" (* field is uninitialized *)
  in
  try Int64.of_string ns'
  with _ ->
    (
      let msg = Printf.sprintf "Wrong INT64 representation, value = '%s' [%08d - %06d]" ns offset len in
      raise (DbfDataInvalid msg)
    )

let bs_to_float offset len bs =
  assert (len <= 18);
  let ns = bs_to_str offset len bs in
  let ns' =
    if String.length ns > 0 then ns else "0.0" (* field is uninitialized *)
  in
  try Float.of_string ns'
  with _ ->
    (
      let msg = Printf.sprintf "Wrong FLOAT representation, value = '%s' [%08d - %06d]" ns offset len in
      raise (DbfDataInvalid msg)
    )

let bs_to_logical offset len bs =
  assert (len = 1);
  match (Bytes.get bs offset) with
  | ' ' | '?' -> (* uninitialized *) false
  | 'Y' | 'y' | 'T' | 't' -> true
  | 'N' | 'n' | 'F' | 'f' -> false
  | '\001' -> true (* fuck you, Siemens IAD *)
  | '\000' -> false (* fuck you, Siemens IAD *)
  | x ->
     (
      let msg = Printf.sprintf "Wrong BOOL representation, value = '%c' [%08d - %06d]" x offset len in
      raise (DbfDataInvalid msg)
    )

let create_int_rider (db : dbf_file) field_index =
  let field = db.info.fields.(field_index) in
  match field.ftype with
  | Number when field.fdec = 0 -> (bs_to_int field.faddr field.flen)
  | Memo -> (bs_to_int field.faddr field.flen)
  | _ -> raise (DbfDataInvalid "Field is not INTEGER")

let create_int64_rider (db : dbf_file) field_index =
  let field = db.info.fields.(field_index) in
  match field.ftype with
  | Number when field.fdec = 0 -> (bs_to_int64 field.faddr field.flen)
  | _ -> raise (DbfDataInvalid "Field is not INTEGER_64")

let create_bstring_rider (db : dbf_file) field_index =
  let field = db.info.fields.(field_index) in
  match field.ftype with
  | Character -> (bs_to_bstr field.faddr field.flen)
  | _ -> raise (DbfDataInvalid "Field is not CHAR(n)")

let create_string_rider (db : dbf_file) field_index =
  let field = db.info.fields.(field_index) in
  match field.ftype with
  | Character -> (bs_to_str field.faddr field.flen)
  | _ -> raise (DbfDataInvalid "Field is not CHAR(n)")

let create_bool_rider (db : dbf_file) field_index =
  let field = db.info.fields.(field_index) in
  match field.ftype with
  | Logical -> (bs_to_logical field.faddr field.flen)
  | _ -> raise (DbfDataInvalid "Field is not LOGICAl/BOOLEAN")

let create_float_rider (db : dbf_file) field_index =
  let field = db.info.fields.(field_index) in
  match field.ftype with
  | Number when field.fdec > 0 ->
      (bs_to_float field.faddr field.flen)
  | Float -> (bs_to_float field.faddr field.flen)
  | _ -> raise (DbfDataInvalid "Field is not FLOAT")


let create_int_rider_by_name (db : dbf_file) fieldname =
  let fid =
    Array.find_index
      (fun (f : dbf_field_descriptor) -> f.name = fieldname)
      db.info.fields
  in
  match fid with
  | Some id -> create_int_rider db id
  | None -> raise (DbfDataInvalid (Printf.sprintf "Fieldname '%s' not found in %s" fieldname db.name))

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