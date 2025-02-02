(* some utilities *)

let bstring_to_hex str =
  String.fold_left (fun t c -> t ^ Printf.sprintf "%0X." (Char.code c)) "" str