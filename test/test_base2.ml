(* tests *)

let test_find_record_1 () =
  let open Dbase4.Base in
  let open Dbase4.Data in
  let dbf = dbf_open "samples/test05.dbf" |> db_go_top in
  let f_id = create_int_rider dbf 0 in
  let res = db_find_record_simple (db_go_top dbf) f_id 5 in
  Alcotest.(check (option int)) "field with ID = 5" (Some(1)) (res);
  let res = db_find_record_simple (db_go_top dbf) f_id 4 in
  Alcotest.(check (option int)) "field with ID = 4" (Some(2)) (res);
  let res = db_find_record_simple (db_go_top dbf) f_id 3 in
  Alcotest.(check (option int)) "field with ID = 3" (Some(3)) (res);
  let res = db_find_record_simple (db_go_top dbf) f_id 2 in
  Alcotest.(check (option int)) "field with ID = 2" (Some(4)) (res);
  let res = db_find_record_simple (db_go_top dbf) f_id 1 in
  Alcotest.(check (option int)) "field with ID = 1" (Some(5)) (res);
  let res = db_find_record_simple dbf f_id 99 in
  Alcotest.(check (option int)) "field ?" (None) (res);
  dbf_close dbf

let test_find_record_2 () =
  let open Dbase4.Base in
  let open Dbase4.Data in
  let dbf = dbf_open "samples/test05.dbf" |> db_go_top in
  let f_id = create_int_rider dbf 0 in
  let res = db_find_record_simple dbf f_id 1 in
  Alcotest.(check (option int)) "field with ID = 1" (Some(5)) (res);
  let res = db_find_record_simple dbf f_id 5 in
  Alcotest.(check (option int)) "field with ID = 5" (Some(1)) (res);
  dbf_close dbf

let test_find_record_3 () =
  let open Dbase4.Base in
  let open Dbase4.Data in
  let dbf = dbf_open "samples/test05.dbf" |> db_go_top in
  let f_utyp = create_int_rider_by_name dbf "UNITTYP" in
  let db_repos dbf r = match r with Some(id) -> db_goto dbf (id + 1) | None -> dbf in
  let res = db_find_record_simple dbf f_utyp 1315058 in
  Alcotest.(check (option int)) "field with UNITTYPE = 1315058 (first)" (Some(3)) (res);
  let res = db_find_record_simple (db_repos dbf res) f_utyp 1315058 in
  Alcotest.(check (option int)) "field with UNITTYPE = 1315058 (second)" (Some(4)) (res);
  dbf_close dbf
  
let () =
  Alcotest.run "dbf-utils"
    [
      ( "dbf-find-record",
        [
          Alcotest.test_case "Test find record, sample 1-a" `Quick
          test_find_record_1;
          Alcotest.test_case "Test find record, sample 1-b" `Quick
          test_find_record_2;
          Alcotest.test_case "Test find record, sample 2-a" `Quick
          test_find_record_3;
        ] );
    ]