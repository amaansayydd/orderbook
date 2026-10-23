type side = Buy | Sell

type order = {
  id : int;
  side : side;
  price : int;               (* integer ticks, e.g. 10150 = 101.50 *)
  mutable remaining : int;   (* goes down as the order fills *)
}

type trade = { buy_id : int; sell_id : int; fill_price : int; fill_qty : int }

module IntMap = Map.Make (Int)

type book = {
  mutable bids : order Queue.t IntMap.t;   (* price -> orders in arrival order *)
  mutable asks : order Queue.t IntMap.t;
  index : (int, order) Hashtbl.t;          (* order id -> order, for cancels *)
  mutable next_id : int;
}
let create () =
  { bids = IntMap.empty; asks = IntMap.empty; index = Hashtbl.create 16; next_id = 1 }

let get_side book side = match side with Buy -> book.bids | Sell -> book.asks

let set_side book side map =
  match side with Buy -> book.bids <- map | Sell -> book.asks <- map

let best_bid book =
  match IntMap.max_binding_opt book.bids with
  | Some (price, _) -> Some price
  | None -> None

let best_ask book =
  match IntMap.min_binding_opt book.asks with
  | Some (price, _) -> Some price
  | None -> None

(* Put an order at the back of the queue for its price, creating the level if needed. *)
let rest_order book order =
  let levels = get_side book order.side in
  let q =
    match IntMap.find_opt order.price levels with
    | Some q -> q
    | None -> Queue.create ()
  in
  Queue.push order q;
  set_side book order.side (IntMap.add order.price q levels);
  Hashtbl.replace book.index order.id order

let opposite = function Buy -> Sell | Sell -> Buy

(* Best price on the side an incoming order trades against:
   the lowest ask for a buy, the highest bid for a sell. *)
let best_opposite book side =
  match side with
  | Buy -> IntMap.min_binding_opt book.asks
  | Sell -> IntMap.max_binding_opt book.bids

let crosses incoming level_price =
  match incoming.side with
  | Buy -> level_price <= incoming.price
  | Sell -> level_price >= incoming.price

(* Fill [incoming] against the opposite side, best price first and oldest
   order first within a price. Invariant: a price level in the map is never
   empty, so [Queue.peek] is always safe. Returns trades newest first. *)
let rec match_incoming book incoming trades =
  if incoming.remaining = 0 then trades
  else
    match best_opposite book incoming.side with
    | None -> trades
    | Some (level_price, _) when not (crosses incoming level_price) -> trades
    | Some (level_price, q) ->
        let resting = Queue.peek q in
        let fill_qty = min incoming.remaining resting.remaining in
        let buy_id, sell_id =
          match incoming.side with
          | Buy -> (incoming.id, resting.id)
          | Sell -> (resting.id, incoming.id)
        in
        let trade = { buy_id; sell_id; fill_price = resting.price; fill_qty } in
        incoming.remaining <- incoming.remaining - fill_qty;
        resting.remaining <- resting.remaining - fill_qty;
        if resting.remaining = 0 then begin
          ignore (Queue.pop q);
          Hashtbl.remove book.index resting.id;
          if Queue.is_empty q then
            let other = opposite incoming.side in
            set_side book other (IntMap.remove level_price (get_side book other))
        end;
        match_incoming book incoming (trade :: trades)

let add_limit book side ~price ~qty =
  if qty <= 0 || price <= 0 then
    invalid_arg "add_limit: price and qty must be positive";
  let id = book.next_id in
  book.next_id <- id + 1;
  let order = { id; side; price; remaining = qty } in
  let trades = List.rev (match_incoming book order []) in
  if order.remaining > 0 then rest_order book order;
  (id, trades)

let print_level label (price, q) =
  let total = Queue.fold (fun acc o -> acc + o.remaining) 0 q in
  Printf.printf "%s %d x %d\n" label price total

(* Asks from highest to lowest, then bids from highest to lowest. *)
let print_book book =
  List.iter (print_level "ASK") (List.rev (IntMap.bindings book.asks));
  List.iter (print_level "BID") (List.rev (IntMap.bindings book.bids))
