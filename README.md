# orderbook

A limit order book and matching engine in OCaml. Orders are matched with
price-time priority, can be partially filled, and can be cancelled.

```ocaml
open Orderbook

let book = create ()
let _ = add_limit book Sell ~price:100 ~qty:5
let _id, trades = add_limit book Buy ~price:101 ~qty:3
(* one trade: 3 @ 100, and 2 left on the ask side at 100 *)
```

## How to run

```bash
opam install dune ppx_expect
dune build
dune test
```

`dune test` prints nothing when every test passes.

## Design

- **Prices are integer ticks** (e.g. `10150` = 101.50), to avoid
  floating-point rounding errors when comparing and matching prices.
- **Each side is a `Map` from price to a `Queue` of orders.** The map keeps
  prices sorted, so the best bid is the largest key and the best ask the
  smallest. The queue at each price keeps orders in arrival order, which gives
  time priority.
- **A `Hashtbl` maps order ids to orders**, so a cancel can find an order's
  side and price directly.
- **Rule: a price level in the map is never empty.** Every function that removes
  an order also removes its price level once the queue is empty. Matching
  relies on this: when it looks at the best opposite price, the front of that
  queue is always a real order.
- The internal representation is hidden behind `lib/orderbook.mli`. Callers only
  see `create`, `add_limit`, `cancel`, `best_bid`, `best_ask` and `print_book`.

### How matching works

When a limit order arrives, it is matched against the opposite side as long as
it has quantity left and the prices cross (a buy crosses asks at or below its
price, a sell crosses bids at or above its price). Each step fills
`min(incoming, resting)` against the oldest order at the best price, **at the
resting order's price**. Fully filled resting orders are removed, and so is
their price level if it becomes empty. Whatever is left of the incoming order
then rests in the book.

## Cost of each operation

With n price levels on a side and k orders at one price:

| Operation | Cost |
|---|---|
| Best bid / best ask | O(log n) |
| Add an order that doesn't trade | O(log n) |
| Each fill while matching | O(log n) |
| Cancel | O(k) — the price level is rebuilt without the order |

## Testing

`test/test_orderbook.ml` uses expect tests (`ppx_expect`) that print the trades
and the book after each scenario:

- resting orders, best bid/ask, totals per price level
- full fill, partial fill of the incoming order, partial fill of the resting order
- time priority within a price level
- sweeping several price levels, stopping at the limit price
- trades happen at the resting order's price
- orders that don't cross just rest; sells hit the highest bid first
- cancel: live order, last order at a level, unknown id, double cancel,
  already-filled order, and matching after a cancel
- invalid price or quantity raises `Invalid_argument`

## Limitations and next steps

- **Faster cancels.** Replace the queue with a doubly linked list per level and
  store each order's node in the index, so a cancel is O(1).
- **Market orders**, and other order types such as immediate-or-cancel.
- **A command-line interface** to send orders and print the book interactively.
- **Property-based tests** that send many random orders and check that the book
  is never left crossed and that quantity is conserved.
