open Orderbook

let print_best book =
  let show = function Some p -> string_of_int p | None -> "none" in
  Printf.printf "best_bid=%s best_ask=%s\n" (show (best_bid book))
    (show (best_ask book))

let print_trades trades =
  List.iter
    (fun t ->
      Printf.printf "TRADE buy=%d sell=%d %d@%d\n" t.buy_id t.sell_id t.fill_qty
        t.fill_price)
    trades

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

let%expect_test "full fill empties the book" =
  let book = create () in
  ignore (add_limit book Sell ~price:100 ~qty:5);
  let _, trades = add_limit book Buy ~price:100 ~qty:5 in
  print_trades trades;
  print_book book;
  print_best book;
  [%expect {|
    TRADE buy=2 sell=1 5@100
    best_bid=none best_ask=none
    |}]

let%expect_test "incoming order partially filled, rest of it rests" =
  let book = create () in
  ignore (add_limit book Sell ~price:100 ~qty:3);
  let _, trades = add_limit book Buy ~price:100 ~qty:5 in
  print_trades trades;
  print_book book;
  [%expect {|
    TRADE buy=2 sell=1 3@100
    BID 100 x 2
    |}]

let%expect_test "resting order partially filled" =
  let book = create () in
  ignore (add_limit book Sell ~price:100 ~qty:5);
  let _, trades = add_limit book Buy ~price:100 ~qty:3 in
  print_trades trades;
  print_book book;
  [%expect {|
    TRADE buy=2 sell=1 3@100
    ASK 100 x 2
    |}]

let%expect_test "time priority: older order at a price fills first" =
  let book = create () in
  ignore (add_limit book Sell ~price:100 ~qty:2);
  ignore (add_limit book Sell ~price:100 ~qty:2);
  let _, trades = add_limit book Buy ~price:100 ~qty:2 in
  print_trades trades;
  print_book book;
  [%expect {|
    TRADE buy=3 sell=1 2@100
    ASK 100 x 2
    |}]

let%expect_test "buy sweeps several ask levels up to its limit" =
  let book = create () in
  ignore (add_limit book Sell ~price:100 ~qty:2);
  ignore (add_limit book Sell ~price:101 ~qty:2);
  ignore (add_limit book Sell ~price:102 ~qty:2);
  let _, trades = add_limit book Buy ~price:101 ~qty:5 in
  print_trades trades;
  print_book book;
  [%expect {|
    TRADE buy=4 sell=1 2@100
    TRADE buy=4 sell=2 2@101
    ASK 102 x 2
    BID 101 x 1
    |}]

let%expect_test "trade happens at the resting order's price" =
  let book = create () in
  ignore (add_limit book Sell ~price:100 ~qty:1);
  let _, trades = add_limit book Buy ~price:105 ~qty:1 in
  print_trades trades;
  print_book book;
  [%expect {| TRADE buy=2 sell=1 1@100 |}]

let%expect_test "no cross: bid below best ask just rests" =
  let book = create () in
  ignore (add_limit book Sell ~price:100 ~qty:1);
  let _, trades = add_limit book Buy ~price:99 ~qty:1 in
  print_trades trades;
  print_book book;
  print_best book;
  [%expect {|
    ASK 100 x 1
    BID 99 x 1
    best_bid=99 best_ask=100
    |}]

let%expect_test "sell hits the highest bid first" =
  let book = create () in
  ignore (add_limit book Buy ~price:99 ~qty:1);
  ignore (add_limit book Buy ~price:100 ~qty:1);
  let _, trades = add_limit book Sell ~price:99 ~qty:1 in
  print_trades trades;
  print_book book;
  [%expect {|
    TRADE buy=2 sell=3 1@100
    BID 99 x 1
    |}]

let%expect_test "cancel a live order" =
  let book = create () in
  let id, _ = add_limit book Buy ~price:100 ~qty:5 in
  ignore (add_limit book Buy ~price:100 ~qty:3);
  Printf.printf "cancel=%b\n" (cancel book id);
  print_book book;
  [%expect {|
    cancel=true
    BID 100 x 3
    |}]

let%expect_test "cancelling the only order at a price removes the level" =
  let book = create () in
  ignore (add_limit book Sell ~price:101 ~qty:1);
  let id, _ = add_limit book Sell ~price:100 ~qty:1 in
  Printf.printf "cancel=%b\n" (cancel book id);
  print_book book;
  print_best book;
  [%expect {|
    cancel=true
    ASK 101 x 1
    best_bid=none best_ask=101
    |}]

let%expect_test "cancel unknown id, or the same id twice" =
  let book = create () in
  let id, _ = add_limit book Buy ~price:100 ~qty:1 in
  Printf.printf "unknown=%b\n" (cancel book 42);
  Printf.printf "first=%b\n" (cancel book id);
  Printf.printf "second=%b\n" (cancel book id);
  [%expect {|
    unknown=false
    first=true
    second=false
    |}]

let%expect_test "cancelling a fully filled order returns false" =
  let book = create () in
  let id, _ = add_limit book Sell ~price:100 ~qty:2 in
  ignore (add_limit book Buy ~price:100 ~qty:2);
  Printf.printf "cancel=%b\n" (cancel book id);
  [%expect {| cancel=false |}]

let%expect_test "after a cancel, the next order in line trades" =
  let book = create () in
  let first, _ = add_limit book Sell ~price:100 ~qty:2 in
  ignore (add_limit book Sell ~price:100 ~qty:2);
  ignore (cancel book first);
  let _, trades = add_limit book Buy ~price:100 ~qty:2 in
  print_trades trades;
  print_book book;
  [%expect {| TRADE buy=3 sell=2 2@100 |}]

let%expect_test "invalid input raises Invalid_argument" =
  let book = create () in
  let try_add side ~price ~qty =
    match add_limit book side ~price ~qty with
    | exception Invalid_argument msg -> print_endline msg
    | _ -> print_endline "no error"
  in
  try_add Buy ~price:100 ~qty:0;
  try_add Sell ~price:0 ~qty:5;
  try_add Buy ~price:100 ~qty:(-1);
  print_book book;
  [%expect {|
    add_limit: price and qty must be positive
    add_limit: price and qty must be positive
    add_limit: price and qty must be positive
    |}]
