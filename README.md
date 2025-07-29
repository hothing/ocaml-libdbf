# Ocaml library for reading .DBF(+.DBT) files

dBase III/IV (.dbf/.dbt) file reader with Memo support


## Examples

### Print on screen all records from DBF-file

```ocaml
open Dbase4
open Dbase4.Data

type person_record = { 
  id   : int;
  name : string;
  age  : int; 
  memo_note : int (* memo reference *)
}

let make_person_record_rider dbf =
  let f_id = create_int_rider dbf 0 in
  let f_name = create_string_rider_by_name dbf "NAME" in
  let f_age = create_int_rider_by_name dbf "AGE" in
  let f_note = create_int_rider_by_name dbf "NOTE" in
  let rider drec = { 
      id = (f_id drec); 
      name = (f_name drec);
      age = (f_age drec);
      memo_note = (f_note drec)
    }
  in
  rider

let read_memo_note dbf r =
  Memo.load_memo dbf r.memo_note

let printer dbf r = 
      let note = read_memo_note dbf r in
      Printf.printf "ID:\t%05d\n" r.id;
      Printf.printf "NAME:\t%05d\n" r.name;
      Printf.printf "AGE:\t%05d\n" r.age;
      Printf.printf "** NOTE **\n%s" note;
      Printf.printf "\n--------------------------\n"

let print_all db_filename  =
  let dbf = Base.dbf_open db_filename in (* open DBF file *) 
  let rider = make_person_record_rider dbf in (* prepare record rider (translator) *)
  Base.all_records dbf (* iterate over all (non-deleted) records *) 
    |> Seq.map rider  (* translate raw-record to the structure *)
    |> Seq.iter (printer dbf); (* print the record *)
  Base.dbf_close dbf (* close DBF-file *)

let () =
  print_all "sample.dbf"

```

### Print on screen selected records from DBF-file


```ocaml
(* ... same as above *)
let print_old db_filename o_age =
  let dbf = Base.dbf_open db_filename in (* open DBF file *) 
  let rider = make_person_record_rider dbf in  (* prepare record rider (translator) *)
  Base.all_records dbf (* iterate over all (non-deleted) records *) 
    |> Seq.map rider (* translate raw-record to the structure *)
    |> Seq.filter (fun r -> r.age >= o_age) (* (!) select only required records *)
    |> Seq.iter (printer dbf); (* ... and print them *)
  Base.dbf_close dbf (* close DBF-file *)

let () =
  print_old "sample.dbf" 40

```
