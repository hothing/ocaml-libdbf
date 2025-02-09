(* The tests *)
open Dbase4.Base
open Dbase4.Data
open Dbase4.Memo

let dbf_data_type =
  let pp_dbf_data_type ppf x = Fmt.pf ppf "%S" (ftype_to_string x) in
  Alcotest.testable pp_dbf_data_type ( = )

let int_ge =
  let pp_int_ge ppf x = Fmt.pf ppf "%d" x in
  Alcotest.testable pp_int_ge ( <= )
  
let int_le =
  let pp_int_ge ppf x = Fmt.pf ppf "%d" x in
  Alcotest.testable pp_int_ge ( >= )
  
let test_open_db_memo () =
  let dbf = dbf_open "samples/memo/test_02.dbf" in
  Alcotest.(check string) "field name" "UDA" dbf.info.fields.(5).name;
  Alcotest.(check dbf_data_type) "field UDA type" Memo dbf.info.fields.(5).ftype;
  Alcotest.(check string) "field name" "SUBBLOCKS" dbf.info.fields.(6).name;
  Alcotest.(check dbf_data_type) "field SUBBLOCKS type" Memo dbf.info.fields.(6).ftype;
  Alcotest.(check bool) "has Memo file" true (match dbf.memo with Some _ -> true | None -> false);
  dbf_close dbf

let test_read_memo_field () =
  let dbf = dbf_open "samples/memo/test_02.dbf" in
  let f_sbb = create_int_rider_by_name dbf "SUBBLOCKS" in
  let drec = db_go_top dbf |> read_record in
  Alcotest.(check int) "MEMO index" 1 (f_sbb drec);
  let data = load_memo dbf (f_sbb drec) in
  Alcotest.(check int) "MEMO data length" 32 (String.length data);
  Alcotest.(check int) "MEMO data hash" 713657925 (String.hash data);
  dbf_close dbf
  
let test_read_memo_field_in_loop () =
    let dbf = dbf_open "samples/memo/test_02.dbf" in
    let f_sbb = create_int_rider_by_name dbf "SUBBLOCKS" in
    let db = ref (db_go_top dbf) in
    let count = ref 0 in
    let cntud = ref 0 in    
    while not (db_bof !db) && (!count < 1000) do
      let drec = read_record !db in
      if not (is_deleted drec) then
        let midx = f_sbb drec in
        let data = load_memo dbf midx in
        Alcotest.(check int_ge) "MEMO data length" 8 (String.length data);
        cntud := !cntud + 1
      else Alcotest.(check pass) "Correct handling of deleted record" () ();
      db := (db_skip !db 1);
      count := !count + 1 
    done;
    Alcotest.(check int) "Records count" 101 !count;
    Alcotest.(check int_le) "Undeleted records count" !count !cntud;
    dbf_close dbf

(* Run it *)
let () =
  Alcotest.run "read-dbt-v4"
    [
      ( "dbf-memo-hdr",
        [
          Alcotest.test_case "Test open DBF with memo" `Quick
          test_open_db_memo;
        ] );
        ( "dbf-memo-read",
        [
          Alcotest.test_case "Test read DBF with memo, (one)" `Quick
            test_read_memo_field;
          Alcotest.test_case "Test read DBF with memo, (in loop)" `Quick
            test_read_memo_field_in_loop;
        ] )
    ]
