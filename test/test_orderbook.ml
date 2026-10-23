open Orderbook

let print_best book =
  let show = function Some p -> string_of_int p | None -> "none" in
  Printf.printf "best_bid=%s best_ask=%s\n" (show (best_bid book))
    (show (best_ask book))

let%expect_test "order rests when book is empty" =
  let book = create () in
  let id, trades = add_limit book Buy ~price:100 ~qty:5 in
  Printf.printf "id=%d trades=%d\n" id (List.length trades);
  print_book book;
  [%expect {|
    id=1 trades=0
    BID 100 x 5
    |}]

let%expect_test "orders on both sides that don't cross" =
  let book = create () in
  ignore (add_limit book Buy ~price:99 ~qty:3);
  ignore (add_limit book Buy ~price:98 ~qty:4);
  ignore (add_limit book Sell ~price:101 ~qty:2);
  ignore (add_limit book Sell ~price:102 ~qty:6);
  print_book book;
  print_best book;
  [%expect {|
    ASK 102 x 6
    ASK 101 x 2
    BID 99 x 3
    BID 98 x 4
    best_bid=99 best_ask=101
    |}]

let%expect_test "two orders at the same price add up" =
  let book = create () in
  ignore (add_limit book Sell ~price:100 ~qty:3);
  ignore (add_limit book Sell ~price:100 ~qty:4);
  print_book book;
  [%expect {| ASK 100 x 7 |}]

let%expect_test "empty book has no best prices" =
  let book = create () in
  print_book book;
  print_best book;
  [%expect {| best_bid=none best_ask=none |}]
