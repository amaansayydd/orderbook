(** A limit order book with price-time priority matching.

    Prices and quantities are positive integers (prices in ticks). *)

type side = Buy | Sell
type trade = { buy_id : int; sell_id : int; fill_price : int; fill_qty : int }
type book

val create : unit -> book

val add_limit : book -> side -> price:int -> qty:int -> int * trade list
(** [add_limit book side ~price ~qty] assigns the order a new id, matches it
    against the opposite side, and rests any unfilled quantity. Returns the id
    and the trades in the order they happened. Raises [Invalid_argument] if
    [price] or [qty] is not positive. *)

val cancel : book -> int -> bool
(** [cancel book id] removes a resting order. Returns [false] if no live order
    has that id. *)

val best_bid : book -> int option
val best_ask : book -> int option

val print_book : book -> unit
(** Prints asks then bids, highest price first, one line per price level with
    the total quantity there. *)
