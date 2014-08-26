open OUnit
open G2048
open Board_utils

let mk_board_test = QCheck.mk_test ~n:1000 ~pp:string_of_board

let check_board_property name ?size (prop : board -> bool) =
  assert QCheck.(run (mk_board_test (arbitrary_board ?size) prop))

let check_full_board_property name ?size (prop : board -> bool) =
  assert QCheck.(run (mk_board_test (arbitrary_full_board ?size) prop))

let test_shift_board_fixpoint () =
  check_board_property "Shifting reaches a fixpoint after width(board) shifts"
    (fun board ->
      let fixed = iter (List.length board) (shift_board L) board in
      shift_board L fixed = fixed)

let test_add_to_full () =
  check_full_board_property "Tiles cannot be added to a fully-populated board"
    (fun board -> insert_tile t2 board = None)

let test_add () =
  check_board_property "Tiles cannot be added to a fully-populated board"
    QCheck.(Prop.((fun board -> not (is_board_full board))
                  ==>
                  (fun board ->
                    insert_tile t2 board <> None)))

(* Some tests for is_board_board *)
let test_is_board_full () =
  begin
    check_full_board_property "Randomly generated full boards are full"
      is_board_full;

    assert_equal true
      (is_board_full []);

    assert_equal true
      (is_board_full [[t2]]);

    assert_equal true
      (is_board_full [[t2; t4 ];
                      [t8; t16]]);

    assert_equal false
      (is_board_full [[empty]]);

    assert_equal false
      (is_board_full [[t2; empty];
                      [t4; t8   ]]);

    assert_equal false
      (is_board_full [[empty; empty];
                      [empty; empty]]);
  end

(* Tests for insert_into_board *)
let test_insert () =
  let insert_property tile board =
    let ofSome = function Some x -> x | None -> assert false in
   (* rely on the fact that `sort_tiles` places empties first *)
    assert (not (is_board_full board));
    (sorted_tiles (board_tiles (ofSome (insert_tile tile board)))
     =
     sorted_tiles (tile :: List.tl (sorted_tiles (board_tiles board))))
  in
  check_board_property "insert_into_board adds a tile to the board"
    QCheck.(Prop.((fun board -> not (is_board_full board))
                     ==>
                  (insert_property t8)))



(* Some tests for movements *)
let test_movements () =
  let board = [[t2   ; empty; t2   ; t4   ];
               [t2   ; empty; empty; t4   ];
               [empty; empty; empty; empty];
               [empty; empty; t8   ; t8   ]]
  in
  begin
    assert_equal (shift_board L board)
      ~printer:string_of_board
      [[t4   ; t4   ; empty; empty];
       [t2   ; t4   ; empty; empty];
       [empty; empty; empty; empty];
       [t16  ; empty; empty; empty]];

    assert_equal (shift_board R board)
      ~printer:string_of_board
      [[empty; empty; t4   ; t4   ];
       [empty; empty; t2   ; t4   ];
       [empty; empty; empty; empty];
       [empty; empty; empty; t16  ]];

    assert_equal (shift_board U board)
      ~printer:string_of_board
      [[t4   ; empty; t2   ; t8   ];
       [empty; empty; t8   ; t8   ];
       [empty; empty; empty; empty];
       [empty; empty; empty; empty]];

    assert_equal (shift_board D board)
      ~printer:string_of_board
      [[empty; empty; empty; empty];
       [empty; empty; empty; empty];
       [empty; empty; t2   ; t8   ];
       [t4   ; empty; t8   ; t8   ]];

    assert_equal (shift_board L (shift_board L board))
      ~printer:string_of_board
      [[t8   ; empty; empty; empty];
       [t2   ; t4   ; empty; empty];
       [empty; empty; empty; empty];
       [t16  ; empty; empty; empty]];

    assert_equal (shift_board U (shift_board U board))
      ~printer:string_of_board
      [[t4   ; empty; t2   ; t16  ];
       [empty; empty; t8   ; empty];
       [empty; empty; empty; empty];
       [empty; empty; empty; empty]];
  end

let suite = "2048 tests" >:::
  ["a fixpoint is reached after width(board) shift_boards"
    >:: test_shift_board_fixpoint;

   "tiles can be added to a board that is not fully-populated"
    >:: test_add;

   "tiles cannot be added to a fully-populated board"
    >:: test_add_to_full;

   "test is_board_full"
    >:: test_is_board_full;

   "test insert_into_board"
    >:: test_insert;

   "test movements"
    >:: test_movements;
  ]
let _ =
  run_test_tt_main suite