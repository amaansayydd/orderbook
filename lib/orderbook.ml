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

let add_limit book side ~price ~qty =
  if qty <= 0 || price <= 0 then
    invalid_arg "add_limit: price and qty must be positive";
  let id = book.next_id in
  book.next_id <- id + 1;
  let order = { id; side; price; remaining = qty } in
  rest_order book order;
  (id, [])

let print_level label (price, q) =
  let total = Queue.fold (fun acc o -> acc + o.remaining) 0 q in
  Printf.printf "%s %d x %d\n" label price total

(* Asks from highest to lowest, then bids from highest to lowest. *)
let print_book book =
  List.iter (print_level "ASK") (List.rev (IntMap.bindings book.asks));
  List.iter (print_level "BID") (List.rev (IntMap.bindings book.bids))
