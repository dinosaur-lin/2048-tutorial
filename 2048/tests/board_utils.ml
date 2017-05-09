(*
 * Copyright (c) 2014 Jeremy Yallop.
 *
 * This file is distributed under the terms of the BSD2 License.
 * See the file COPYING for details.
 *)

open G2048

let current_stage = 5

(** Formatting for boards *)
let repeat_string n s =
  let len = String.length s in
  let buf = Buffer.create (n * len) in
  for i = 1 to n do Buffer.add_string buf s done;
  Buffer.contents buf

let pad n s = Printf.sprintf "%*s " n s

let string_of_board b =
  let pad_square t = pad 5 (string_of_square t) in
  let format_row row =
    Printf.sprintf "|%s|" (String.concat "" (List.map pad_square row))
  in
  let hline =
    match b with
    | b :: _ -> repeat_string (List.length b) "------"
    | _      -> repeat_string (List.length b) "------"
  in
  let body = String.concat "\n" (List.map format_row b) in
  "\n " ^ hline ^ "\n" ^ body ^ "\n " ^ hline

let string_of_provenances b =
  let string_of_prov {value; shift} =
    Printf.sprintf "<-{%d}-%d" shift value in
  let string_of_provs = function
    | [] -> pad 15 "-"
    | provs -> pad 15 (String.concat "+" (List.map string_of_prov provs)) in
  let string_of_provenance_row row =
    String.concat " " (List.map string_of_provs row)
  in "\n"^ String.concat "\n" (List.map string_of_provenance_row b)

(** Generating random boards *)
let non_empty_squares = [t2; t4; t8; t16; t32; t64; t128; t256; t512; t1024; t2048]
let squares = empty :: non_empty_squares

let const x _ = x

let gen_square : square QCheck.Gen.t =
  QCheck.Gen.oneofl squares  

let gen_non_empty_square: square QCheck.Gen.t =
  QCheck.Gen.oneofl non_empty_squares

let gen_row ~len : square list QCheck.Gen.t =
  QCheck.Gen.list_size (const len) gen_square
  
let gen_rull_row ~len : square list QCheck.Gen.t =
  QCheck.Gen.list_size (const len) gen_non_empty_square

let gen_board ?(size=4): board QCheck.Gen.t =
  QCheck.Gen.list_size (const size) (gen_row ~len:size)

let gen_full_board ?(size=4): board QCheck.Gen.t =
  QCheck.Gen.list_size (const size) (gen_rull_row ~len:size)

let arbitrary_square : square QCheck.arbitrary =
  QCheck.make gen_square

let arbitrary_non_empty_square : square QCheck.arbitrary =
  QCheck.make gen_non_empty_square

let arbitrary_row ~len : square list QCheck.arbitrary =
  QCheck.make @@ gen_row ~len:len

let arbitrary_full_row ~len : square list QCheck.arbitrary =
  QCheck.make @@ gen_rull_row ~len:len
  
let arbitrary_board ?(size=4) : board QCheck.arbitrary =
  QCheck.make gen_board

let arbitrary_full_board ?(size=4) : board QCheck.arbitrary =
  QCheck.make gen_full_board

let rec iter n f x = if n = 0 then x else iter (n - 1) f (f x)

let sorted_squares : int option list -> int option list = List.sort Pervasives.compare
let board_squares = List.concat

let board_map f b = List.map (List.map f) b

let square_equal l r = square_value l = square_value r
let row_equal r1 r2 =
    List.length r1 = List.length r2
 && List.for_all2 square_equal r1 r2
let board_equal b1 b2 =
    List.length b1 = List.length b2
 && List.for_all2 row_equal b1 b2

let board_value_list b =
  List.sort Pervasives.compare
   (board_squares (board_map square_value b))

let is_row_full r = not (List.exists ((=)empty) r)

let is_board_full b = List.for_all is_row_full b
