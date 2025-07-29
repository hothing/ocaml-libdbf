(* The tests *)

let test_read_rec () =
  let open Dbase4.Base in
  let open Dbase4.Data in
  let dbf = dbf_open "samples/test.dbf" in
  let f_id = create_int_rider dbf 0 in
  let f_name = create_string_rider dbf 1 in
  let f_intversion = create_int_rider_by_name dbf "INTVERSION" in
  let _, drec = db_go_top dbf |> read_record in
  Alcotest.(check int) "field#1 value" 1 (f_id drec);
  Alcotest.(check string) "field#2 value" "eaf" (f_name drec |> String.trim);
  Alcotest.(check int) "field#3 value" 100 (f_intversion drec);
  dbf_close dbf

let test_read_rec_fail () =
  let open Dbase4.Base in
  let dbf = dbf_open "samples/test.dbf" in
  (try
     let _ = db_goto dbf 100 |> read_record in
     Alcotest.fail "Index set up after last record"
   with _ -> Alcotest.(check pass) "Correct exception" () ());
  dbf_close dbf

let test_read_rec_5 () =
  let open Dbase4.Base in
  let open Dbase4.Data in
  let dbf = dbf_open "samples/test05.dbf" in
  let f_id = create_int_rider dbf 0 in
  let f_objtyp = create_int_rider dbf 1 in
  let f_unitid = create_int_rider_by_name dbf "UNITID" in
  let f_unittyp = create_int_rider_by_name dbf "UNITTYP" in
  let _, drec = db_go_top dbf |> read_record in
  Alcotest.(check int) "field#1 value" 5 (f_id drec);
  Alcotest.(check int) "field#2 value" 1316109 (f_objtyp drec);
  Alcotest.(check int) "field#3 value" 2 (f_unitid drec);
  Alcotest.(check int) "field#4 value" 1314974 (f_unittyp drec);
  dbf_close dbf

let bstring_to_hex str =
  String.fold_left (fun t c -> t ^ Printf.sprintf "%0X." (Char.code c)) "" str

let test_read_rec_5_1 () =
  let open Dbase4.Base in
  let open Dbase4.Data in
  let dbf = dbf_open "samples/test05.dbf" in
  let f_id = create_int_rider dbf 0 in
  let f_objtyp = create_int_rider dbf 1 in
  let f_unitid = create_int_rider_by_name dbf "UNITID" in
  let f_unittyp = create_int_rider_by_name dbf "UNITTYP" in
  let f_name = create_string_rider_by_name dbf "NAME" in
  let f_cdate = create_bstring_rider_by_name dbf "CREATE" in
  let _, drec = db_goto dbf 3 |> read_record in
  Alcotest.(check int) "field#1 value" 3 (f_id drec);
  Alcotest.(check int) "field#2 value" 1316109 (f_objtyp drec);
  Alcotest.(check int) "field#3 value" 1 (f_unitid drec);
  Alcotest.(check int) "field#4 value" 1315058 (f_unittyp drec);
  Alcotest.(check string) "field#5 value" "AI8x12Bit" (f_name drec);
  Alcotest.(check string)
    "field#6 value" "3.7C.57.43.35.EE.52.0.20.20."
    (f_cdate drec |> bstring_to_hex);
  dbf_close dbf

(* Run it *)
let () =
  Alcotest.run "read-dbf-v3"
    [
      ( "dbf-read-data",
        [
          Alcotest.test_case "Test read 1 record" `Quick test_read_rec;
          Alcotest.test_case "Test read 1 record (must fail)" `Quick
            test_read_rec_fail;
          Alcotest.test_case "Test read 1 record, sample 5" `Quick
            test_read_rec_5;
          Alcotest.test_case "Test read 1 record, sample 5, record 3" `Quick
            test_read_rec_5_1;
        ] );
    ]
