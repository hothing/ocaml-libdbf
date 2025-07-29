(* tests *)

let test_iterator_1 () =
  let open Dbase4.Base in
  let open Dbase4.Data in
  let dbf = dbf_open "samples/test05.dbf" |> db_go_top in
  let f_id = create_int_rider dbf 0 in
  let iter = iterator dbf in
  let test_id id rr =
    let _rid, _del, _data = rr in
    if not _del then
      let x_id = f_id _data in
      if x_id = id then Some(x_id) else None 
    else None
  in
  let res = Seq.find_map (test_id 1) iter in
  Alcotest.(check (option int)) "field with ID = 1" (Some(1)) (res);
  let res = Seq.find_map (test_id 99) iter in
  Alcotest.(check (option int)) "not existed record with ID = 99" (None) (res);
  let res = Seq.find_map (test_id 3) iter in
  Alcotest.(check (option int)) "field with ID = 3" (Some(3)) (res);
  let res = Seq.find_map (test_id 4) iter in
  Alcotest.(check (option int)) "field with ID = 4" (Some(4)) (res);
  dbf_close dbf

let test_iterator_2 () =
  let open Dbase4.Base in
  let open Dbase4.Data in
  let dbf = dbf_open "samples/test05.dbf" |> db_go_top in
  let f_id = create_int_rider dbf 0 in
  let f_utyp = create_int_rider_by_name dbf "UNITTYP" in
  let iter = all_records dbf in
  let test_id id rr =
    let x_id = f_id rr in
    let x_ut = f_utyp rr in
    if (x_id = id) then Some(x_ut) else None
  in
  let res = Seq.find_map (test_id 1) iter in
  Alcotest.(check (option int)) "record with ID = 1" (Some(1315058)) (res);
  let res = Seq.find_map (test_id 4) iter in
  Alcotest.(check (option int)) "record with ID = 4" (Some(1314974)) (res);
  let res = Seq.find_map (test_id 3) iter in
  Alcotest.(check (option int)) "record with ID = 3" (Some(1315058)) (res);
  let res = Seq.find_map (test_id 99) iter in
  Alcotest.(check (option int)) "not existed record with ID = 99" (None) (res);
  dbf_close dbf
  
let () =
  Alcotest.run "dbf-utils"
    [
      ( "dbf-find-record",
        [
          Alcotest.test_case "Test find record, sample 1-a" `Quick
          test_iterator_1;
          Alcotest.test_case "Test find record, sample 1-b" `Quick
          test_iterator_2;
        ] );
    ]