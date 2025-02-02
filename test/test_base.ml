(* The tests *)
open Dbf.Base

let x_decode_hdr0 filename =
  let open Dbf.Base in
  let fin = In_channel.open_bin filename in
  let hsz = Header.sizeof_t in
  read_chunk fin hsz |> decode_hdr0

let test_read_hdr0_1 () =
  let open Dbf.Base in
  let hdr0 = x_decode_hdr0 "samples/test.dbf" in
  let yy, mm, dd = hdr0.mdate in
  Alcotest.(check int) "version" 3 hdr0.version;
  Alcotest.(check int) "header size" 673 hdr0.hdr_size;
  Alcotest.(check int) "record count" 1 hdr0.num_records;
  Alcotest.(check int) "record size" 594 hdr0.rec_size;
  Alcotest.(check int) "m'date year" 2021 yy;
  Alcotest.(check int) "m'date month" 10 mm;
  Alcotest.(check int) "m'date day" 19 dd

let test_read_hdr0_2 () =
  let open Dbf.Base in
  let hdr0 = x_decode_hdr0 "samples/test02.dbf" in
  let yy, mm, dd = hdr0.mdate in
  Alcotest.(check int) "version" 3 hdr0.version;
  Alcotest.(check int) "header size" 673 hdr0.hdr_size;
  Alcotest.(check int) "record count" 2 hdr0.num_records;
  Alcotest.(check int) "record size" 496 hdr0.rec_size;
  Alcotest.(check int) "m'date year" 2021 yy;
  Alcotest.(check int) "m'date month" 11 mm;
  Alcotest.(check int) "m'date day" 24 dd

let test_read_hdr0_3 () =
  let open Dbf.Base in
  let hdr0 = x_decode_hdr0 "samples/test03.dbf" in
  let yy, mm, dd = hdr0.mdate in
  Alcotest.(check int) "version" 0x8B hdr0.version;
  Alcotest.(check int) "header size" 833 hdr0.hdr_size;
  Alcotest.(check int) "record count" 0 hdr0.num_records;
  Alcotest.(check int) "record size" 192 hdr0.rec_size;
  Alcotest.(check int) "m'date year" 2021 yy;
  Alcotest.(check int) "m'date month" 10 mm;
  Alcotest.(check int) "m'date day" 19 dd

let test_read_hdr0_4 () =
  let open Dbf.Base in
  let hdr0 = x_decode_hdr0 "samples/test04.dbf" in
  let yy, mm, dd = hdr0.mdate in
  Alcotest.(check int) "version" 0x8B hdr0.version;
  Alcotest.(check int) "header size" 257 hdr0.hdr_size;
  Alcotest.(check int) "record count" 0 hdr0.num_records;
  Alcotest.(check int) "record size" 93 hdr0.rec_size;
  Alcotest.(check int) "m'date year" 2021 yy;
  Alcotest.(check int) "m'date month" 10 mm;
  Alcotest.(check int) "m'date day" 19 dd

let test_read_hdr0_5 () =
  let open Dbf.Base in
  let hdr0 = x_decode_hdr0 "samples/test05.dbf" in
  let yy, mm, dd = hdr0.mdate in
  Alcotest.(check int) "version" 3 hdr0.version;
  Alcotest.(check int) "header size" 609 hdr0.hdr_size;
  Alcotest.(check int) "record count" 5 hdr0.num_records;
  Alcotest.(check int) "record size" 505 hdr0.rec_size;
  Alcotest.(check int) "m'date year" 2021 yy;
  Alcotest.(check int) "m'date month" 10 mm;
  Alcotest.(check int) "m'date day" 19 dd

let x_decode_hdr1 filename =
  let open Dbf.Base in
  let fin = In_channel.open_bin filename in
  let hsz = 0x20 in
  let hdr0 = read_chunk fin hsz |> decode_hdr0 in
  let fsz = hdr0.hdr_size - hsz in
  let _ = In_channel.seek fin (Int64.of_int hsz) in
  let fields = read_chunk fin fsz |> decode_struct fsz |> Array.of_list in
  In_channel.close fin;
  { hdr0 with fields }

let test_read_hdr1_1 () =
  let hdr = x_decode_hdr1 "samples/test.dbf" in
  Alcotest.(check int)
    "record size (calculated)"
    (calculate_rec_size hdr.fields)
    hdr.rec_size;
  Alcotest.(check string) "field#1 name" "ID" hdr.fields.(0).name;
  Alcotest.(check int) "field#1 length" 11 hdr.fields.(0).flen

let test_read_hdr1_2 () =
  let hdr = x_decode_hdr1 "samples/test02.dbf" in
  Alcotest.(check int)
    "record size (calculated)"
    (calculate_rec_size hdr.fields)
    hdr.rec_size;
  Alcotest.(check string) "field#1 name" "ID" hdr.fields.(0).name;
  Alcotest.(check int) "field#1 length" 11 hdr.fields.(0).flen

let test_read_hdr1_3 () =
  let hdr = x_decode_hdr1 "samples/test03.dbf" in
  Alcotest.(check int)
    "record size (calculated)"
    (calculate_rec_size hdr.fields)
    hdr.rec_size;
  Alcotest.(check string) "field#1 name" "OBJECTID" hdr.fields.(0).name;
  Alcotest.(check int) "field#1 length" 11 hdr.fields.(0).flen

let test_read_hdr1_4 () =
  let hdr = x_decode_hdr1 "samples/test04.dbf" in
  Alcotest.(check int)
    "record size (calculated)"
    (calculate_rec_size hdr.fields)
    hdr.rec_size;
  Alcotest.(check string) "field#1 name" "ID" hdr.fields.(0).name;
  Alcotest.(check int) "field#1 length" 11 hdr.fields.(0).flen

let test_read_hdr1_5 () =
  let hdr = x_decode_hdr1 "samples/test05.dbf" in
  Alcotest.(check int)
    "record size (calculated)"
    (calculate_rec_size hdr.fields)
    hdr.rec_size;
  Alcotest.(check string) "field#1 name" "ID" hdr.fields.(0).name;
  Alcotest.(check int) "field#1 length" 11 hdr.fields.(0).flen;
  Alcotest.(check string) "field#2 name" "OBJTYP" hdr.fields.(1).name;
  Alcotest.(check string) "field#17 name" "CREATE" hdr.fields.(16).name

let test_read_hdr () =
  let dbf = Dbf.Base.dbf_open "samples/test.dbf" in
  Array.iter
    (fun (f : Dbf.Base.dbf_field_descriptor) ->
      Printf.printf "'%s' %d %d\n" f.name f.flen f.faddr)
    dbf.info.fields;
  Alcotest.(check string) "field name" "RSRVD12_C" dbf.info.fields.(19).name;
  dbf_close dbf

let test_read_rec () =
  let open Dbf.Base in
  let open Dbf.Data in
  let dbf = dbf_open "samples/test.dbf" in
  let f_id = create_int_rider dbf 0 in
  let f_name = create_string_rider dbf 1 in
  let f_intversion = create_int_rider_by_name dbf "INTVERSION" in
  let drec = db_go_top dbf |> read_record in
  Alcotest.(check int) "field#1 value" 1 (f_id drec);
  Alcotest.(check string) "field#2 value" "eaf" (f_name drec |> String.trim);
  Alcotest.(check int) "field#3 value" 100 (f_intversion drec);
  dbf_close dbf

let test_read_rec_fail () =
  let open Dbf.Base in
  let dbf = dbf_open "samples/test.dbf" in
  (try
     let _ = db_goto dbf 100 |> read_record in
     Alcotest.fail "Index set up after last record"
   with _ -> Alcotest.(check pass) "Correct exception" () ());
  dbf_close dbf

let test_read_rec_5 () =
  let open Dbf.Base in
  let open Dbf.Data in
  let dbf = dbf_open "samples/test05.dbf" in
  let f_id = create_int_rider dbf 0 in
  let f_objtyp = create_int_rider dbf 1 in
  let f_unitid = create_int_rider_by_name dbf "UNITID" in
  let f_unittyp = create_int_rider_by_name dbf "UNITTYP" in
  let drec = db_go_top dbf |> read_record in
  Alcotest.(check int) "field#1 value" 5 (f_id drec);
  Alcotest.(check int) "field#2 value" 1316109 (f_objtyp drec);
  Alcotest.(check int) "field#3 value" 2 (f_unitid drec);
  Alcotest.(check int) "field#4 value" 1314974 (f_unittyp drec);
  dbf_close dbf

let bstring_to_hex str =
  String.fold_left (fun t c -> t ^ Printf.sprintf "%0X." (Char.code c)) "" str

let test_read_rec_5_1 () =
  let open Dbf.Base in
  let open Dbf.Data in
  let dbf = dbf_open "samples/test05.dbf" in
  let f_id = create_int_rider dbf 0 in
  let f_objtyp = create_int_rider dbf 1 in
  let f_unitid = create_int_rider_by_name dbf "UNITID" in
  let f_unittyp = create_int_rider_by_name dbf "UNITTYP" in
  let f_name = create_string_rider_by_name dbf "NAME" in
  let f_cdate = create_bstring_rider_by_name dbf "CREATE" in
  let drec = db_goto dbf 3 |> read_record in
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
      ( "dbf-read-decode-header0",
        [
          Alcotest.test_case "Test read header 0, sample 1" `Quick
            test_read_hdr0_1;
          Alcotest.test_case "Test read header 0, sample 2" `Quick
            test_read_hdr0_2;
          Alcotest.test_case "Test read header 0, sample 3" `Quick
            test_read_hdr0_3;
          Alcotest.test_case "Test read header 0, sample 4" `Quick
            test_read_hdr0_4;
          Alcotest.test_case "Test read header 0, sample 5" `Quick
            test_read_hdr0_5;
        ] );
      ( "dbf-read-decode-header1",
        [
          Alcotest.test_case "Test read header 1, sample 1" `Quick
            test_read_hdr1_1;
          Alcotest.test_case "Test read header 1, sample 2" `Quick
            test_read_hdr1_2;
          Alcotest.test_case "Test read header 1, sample 3" `Quick
            test_read_hdr1_3;
          Alcotest.test_case "Test read header 1, sample 4" `Quick
            test_read_hdr1_4;
          Alcotest.test_case "Test read header 1, sample 5" `Quick
            test_read_hdr1_5;
          Alcotest.test_case "Test read full header" `Quick test_read_hdr;
        ] );
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
