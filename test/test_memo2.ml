open Dbase4.Base
open Dbase4.Data
open Dbase4.Memo

let test_read_memo_field_sbb () =
  let dbf = Dbase4.Base.dbf_open "samples/memo/test_02.dbf" in
  let f_sbb = create_int_rider_by_name dbf "SUBBLOCKS" in
  let db = ref (db_go_top dbf) in
  let count = ref 0 in
  let cntud = ref 0 in    
  while not (db_bof !db) && (!count < 1000) do
    let drec = read_record !db in
    if not (is_deleted drec) then
      let midx = f_sbb drec in
      let data = load_memo dbf midx in
      Printf.printf "%d -> %d\n" !count (String.length data);
      cntud := !cntud + 1
    else ();
    db := (db_skip !db 1);
    count := !count + 1 
  done;
  dbf_close dbf

let test_read_memo_field_uda () =
  let dbf = Dbase4.Base.dbf_open "samples/memo/test_02.dbf" in
  let f_sbb = create_int_rider_by_name dbf "UDA" in
  let db = ref (db_go_top dbf) in
  let count = ref 0 in
  let cntud = ref 0 in    
  while not (db_bof !db) && (!count < 1000) do
    let drec = read_record !db in
    count := !count + 1 ;
    if not (is_deleted drec) then
      let midx = f_sbb drec in
      let data = load_memo dbf midx in
      Printf.printf "%d - %d - %s\n" !count (String.length data) (String.escaped data);
      cntud := !cntud + 1
    else ();
    db := (db_skip !db 1);
  done;
  dbf_close dbf

let () = test_read_memo_field_sbb() ; test_read_memo_field_uda()