
type (_, _) eq = Refl : ('a, 'a) eq

module Handler_index : sig

  type ('es1, 'es2) t [@@immediate]
  (** [(es1, es2) t] represents an index into the effect list [es2]. [es1]
      is the tail sublist containing the index and all following effects.
      For instance, [('b * ('c * unit), 'a * ('b * ('c * unit)) t] represents
      effect 1, ['b].

      It is used for building [Raw_handler.t]s, which represent one of the
      effects handled by a particular handler. *)

  val zero : ('es, 'es) t
  (** [zero] is the index of the first element of an effect list. *)

  val one : ('es, 'e * 'es) t
  (** [zero] is the index of the second element of an effect list. *)

  val succ : ('e * 'es1, 'es2) t -> ('es1, 'es2) t
  (** [succ t] is the index of the next element of an effect list after [t]. *)

  val is_zero : ('es1, 'es2) t -> ('es1, 'es2) eq option
  (** [is_zero t] tests if [t] is the index of the first element of an effect
      list. If it is then we learn that the remaining list is equal to the
      full list. *)

  val weaken : ('es1, 'e * 'es2) t -> ('es1, 'es2) t
  (** [weaken t] is an index for [es], where [t] is an index for [e * es].
      If [t] is [zero] then [weaken t] is an invalid index and does not
      correspond to an element in [es]. *)

  val to_int : ('es1, 'es2) t -> int
  (** [to_int t] is the integer representation of [t]. *)

  val is_invalid : ('es1, 'es2) t -> bool
  (** [is_invalid t] is [true] if [t] is an invalid index produced by
      [weaken]. *)

end = struct

  type ('es1, 'es2) t = int

  let zero = 0

  let one = 1

  let succ t = t + 1

  let is_zero (type es1 es2) (t : (es1, es2) t) =
    if Int.equal t 0 then (Obj.magic (Some Refl) : (es1, es2) eq option)
    else None

  let weaken t = t - 1

  let to_int t = t

  let is_invalid t = (t < 0)

end

module Raw_handler : sig

  type ('e, 'es) t [@@immediate]
  (** [(e, es) t] is a handler for the effect [e], which must be an element
      of the effect list [es].

      It is used to represent one of the effects handled by a particular
      handler. *)

  val of_index : ('e * _, 'es) Handler_index.t -> ('e, 'es) t
  (** [of_index i] is the handler corresponding to index [t]. *)

  val zero : ('e, 'e * _) t
  (** [zero] is [of_index (Handler_index.zero)]. *)

  val is_zero : ('e1, 'e2 * _) t -> ('e1, 'e2) eq option
  (** [is_zero t] tests if this handler is [zero]. If it is we learn
      that the handled effect is the first effect in the list. *)

  val weaken : ('e1, 'e2 * 'es2) t -> ('e1, 'es2) t
  (** [weaken t] is a handler from the list [es], where [t] is a handler
      from the list [e * es]. If [t] is [zero] then [weaken t] is an
      invalid handler (i.e. not actually in the list). *)

  val to_int : ('e, 'es) t -> int
  (** [to_int t] is the integer representation of [t]. *)

  val is_invalid : ('e, 'es2) t -> bool
  (** [is_invalid t] is [true] if [t] is an invalid handler produced by
      [weaken]. *)

end = struct

  type ('e, 'es) t =
      Raw_handler : ('e * _, 'es) Handler_index.t -> ('e, 'es) t
  [@@unboxed]

  let of_index i = Raw_handler i

  let zero = Raw_handler Handler_index.zero

  let is_zero (type e1 e2 es) (Raw_handler i : (e1, e2 * es) t)
    : (e1, e2) eq option =
    match Handler_index.is_zero i with
    | Some Refl as eq -> eq
    | None as eq -> eq

  let weaken (Raw_handler i) = Raw_handler (Handler_index.weaken i)

  let to_int (Raw_handler i) = Handler_index.to_int i

  let is_invalid (Raw_handler i) = Handler_index.is_invalid i

end

module Handler : sig

  type 'e t [@@immediate]
  (** [e t] is a handler for the effect [e]. *)

  val of_raw : ('e, 'es) Raw_handler.t -> 'e t
  (** [of_raw h] is the handler corresponding to the raw handler [h]. *)

  external unsafe_to_raw
    : ('es1 t [@local_opt]) -> (('es1, 'es2) Raw_handler.t[@local_opt])
    = "%identity"
  (** [unsafe_of_raw t] is the raw representation of [t]. This is only
      safe if the resulting raw handler has the same effect list as
      the raw handler used to create [t] via [of_raw]. *)

end = struct

  type 'e t [@@immediate]

  let of_raw (type e es) (h : (e, es) Raw_handler.t) =
    (Obj.magic h : e t)

  external unsafe_to_raw
    : ('es1 t [@local_opt]) -> (('es1, 'es2) Raw_handler.t[@local_opt])
    = "%identity"

end

module Length = struct

  type x = X
  (** [x] is the type of [X]s *)

  type 'es t =
    | [] : unit t
    | (::) : x * 'es t -> ('e * 'es) t
  (** [es t] is the length of effect list [es]. It has slightly unusual
      constructors so that lengths can be written as [[X;X;X]] rather
      than e.g. [(S (S (S Z)))]. This looks nicer on calls to [fiber_with]:

      {[
        fiber_with [X; X; X] (fun [a; b; c] -> ...)
      ]}
  *)

end

module Raw_handlers : sig

  type ('es1, 'es2) t =
    | [] : (unit, 'es) t
    | (::) : ('e, 'es2) Raw_handler.t * ('es1, 'es2) t -> ('e * 'es1, 'es2) t
  (** [(es1, es2) t] is a list of handlers for effects [es1]. These handlers are
      all selected from effect list [es2]. *)

  val initial : length:(*local_*) 'es Length.t -> ('es, 'e * 'es) t
  (** [initial ~length] is a list of handlers for effects [es], where [length]
      is the length of [es]. These handlers are selected from the effect list
      [e * es] for some effect [e]. *)

  val initial_from : (*local_*) ('es, 'eso) t -> ('es, 'e * 'es) t
  (** [initial_from hs] is a list of handlers for effect [es], where [hs] is
      another list of handlers for effects [es]. These handlers are selected
      from the effect list [e * es] for some effect [e]. Note that [hs] is
      being used only for its length -- the actual handlers in it do not
      affect the output. *)

end = struct

  type ('es1, 'es2) t =
    | [] : (unit, 'es) t
    | (::) : ('e, 'es2) Raw_handler.t * ('es1, 'es2) t -> ('e * 'es1, 'es2) t

  let initial (type e es) ~((*local_*) length : es Length.t) : (es, e * es) t =
    let rec loop : type esr.
      (esr, e * es) Handler_index.t -> (*local_*) esr Length.t -> (esr, e * es) t =
      fun i l ->
        match l with
        | [] -> []
        | X :: l' ->
            let h = Raw_handler.of_index i in
            h :: loop (Handler_index.succ i) l'
    in
    loop Handler_index.one length [@nontail]

  let initial_from (type e es eso) ((*local_*) t : (es, eso) t) : (es, e * es) t =
    let rec loop : type esr.
      (esr, e * es) Handler_index.t -> (*local_*) (esr, eso) t -> (esr, e * es) t =
      fun i l ->
        match l with
        | [] -> []
        | _ :: rest ->
            let h = Raw_handler.of_index i in
            h :: loop (Handler_index.succ i) rest
    in
    loop Handler_index.one t [@nontail]

end

module Handlers : sig

  type 'es t =
    | [] : unit t
    | (::) : 'e Handler.t * 'es t -> ('e * 'es) t
  (** [es t] is a list of handlers for effects [es]. *)

  module Length = Length

  val length : (*local_*) 'es t -> 'es Length.t
  (** [length t] is the length of [t]. *)

  val of_raw : ('es1, 'es2) Raw_handlers.t -> 'es1 t
  (** [of_raw hs] is the list of handlers corresponding to list of raw
      handlers [hs]. *)

  external unsafe_to_raw
    : ('es1 t [@local_opt]) -> (('es1, 'es2) Raw_handlers.t[@local_opt])
    = "%identity"
  (** [unsafe_of_raw t] is the list of raw handlers corresponding to [t]. This
      is only safe if the resulting raw handlers have the same effect list as
      the raw handlers used to create [t] via [of_raw]. *)

end = struct

  type 'es t =
    | [] : unit t
    | (::) : 'e Handler.t * 'es t -> ('e * 'es) t

  module Length = Length

  let length ((*local_*) t) =
    let rec loop : type es . (*local_*) es t -> es Length.t = function
      | [] -> []
      | _ :: rest -> X :: (loop rest)
    in
    loop t [@nontail]

  let of_raw (type es1 es2) (h : (es1, es2) Raw_handlers.t) =
    (Obj.magic h : es1 t)

  external unsafe_to_raw
    : ('es1 t [@local_opt]) -> (('es1, 'es2) Raw_handlers.t[@local_opt])
    = "%identity"

end

module Mapping : sig

  type ('es1, 'es2) t
  (** [(es1, es2) t] represents a mutable mapping of handlers selected from
      list [es1] to handlers selected from list [es2]. *)

  val lookup :
    (*local_*) ('e, 'es1) Raw_handler.t
    -> ('es1, 'es2) t
    -> ('e, 'es2) Raw_handler.t
  (** [lookup h t] looks up handler [h] in mapping [t] and returns the
      corresponding handler. *)

  val empty : (unit, 'es) t
  (** [empty] is the mapping out of the empty list. *)

  val create : (*local_*) ('es1, 'es2) Raw_handlers.t -> ('es1, 'es2) t
  (** [create hs] creates a new mapping from [es1] to [es2], where [hs]
      is a list of handlers for effects [es1] selected from [es2]. Its
      initial value is to map each handler from [es1] to the corresponding
      handler in [hs]. *)

  val set : (*local_*) ('es1, 'es2) Raw_handlers.t -> ('es1, 'es2) t -> unit
  (** [set hs t] updates the mapping [t] to map each handler to the
      corresponding handler in [hs]. *)

  val create_unset : (*local_*) 'es1 Length.t -> ('es1, 'es2) t
  (** [create len] creates a new uninitialized mapping from [es1] to [es2],
      where [len] is the length of [es1]. The mapping must be initialized with
      [set] before it is used. *)

end = struct

  type element

  let uninitialized : element = Obj.magic (-1)

  type ('es1, 'es2) t = element array
  (* Can be thought of as an [(exists e. (e, 'es2) Raw_handler.t) array] *)

  let lookup (type e es1 es2) (h : (e, es1) Raw_handler.t) (t : (es1, es2) t) =
    let elt = Array.unsafe_get t (Raw_handler.to_int h) in
    (Obj.magic elt : (e, es2) Raw_handler.t)

  let empty = [||]

  let make (type es1 es2) (idx : (unit, es1) Handler_index.t) : (es1, es2) t =
    Array.make (Handler_index.to_int idx) uninitialized

  let set_element (type e esr es1 es2) (t : (es1, es2) t)
      (idx : (e * esr, es1) Handler_index.t) (h : (e, es2) Raw_handler.t) =
    let elt : element = Obj.magic h in
    Array.unsafe_set t (Handler_index.to_int idx) elt

  let create (type es1 es2) ((*local_*) l : (es1, es2) Raw_handlers.t) =
    let rec loop : type es.
      (es, es1) Handler_index.t -> (*local_*) (es, es2) Raw_handlers.t
      -> (es1, es2) t =
      fun idx l ->
        match l with
        | [] -> make idx
        | h :: rest ->
          let t = loop (Handler_index.succ idx) rest in
          set_element t idx h;
          t
    in
    loop Handler_index.zero l [@nontail]

  let set (type es1 es2) ((*local_*) hs) t =
    let rec loop : type es.
      (es, es1) Handler_index.t -> (*local_*) (es, es2) Raw_handlers.t
      -> (es1, es2) t -> unit =
      fun idx hs t ->
        match hs with
        | [] -> ()
        | h :: rest ->
            set_element t idx h;
            loop (Handler_index.succ idx) rest t
    in
    loop Handler_index.zero hs t [@nontail]

  let create_unset (type es1 es2) ((*local_*) l : es1 Length.t) =
    let rec loop : type es.
      (es, es1) Handler_index.t -> (*local_*) es Length.t
      -> (es1, es2) t =
      fun idx l ->
        match l with
        | [] -> make idx
        | X :: l' -> loop (Handler_index.succ idx) l'
    in
    loop Handler_index.zero l [@nontail]

end

type ('a, 'e) op

type ('a, 'e) perform = ('a, 'e) op * 'e Handler.t

external perform : ('a, 'e) perform -> 'a = "%perform"

type (-'a, +'b) stack

external resume : ('a, 'b) stack -> ('c -> 'a) -> 'c -> 'b = "%resume"
external runstack : ('a, 'b) stack -> ('c -> 'a) -> 'c -> 'b = "%runstack"

type (-'a, +'b) cont

external take_cont_noexc : ('a, 'b) cont -> ('a, 'b) stack =
  "caml_continuation_use_noexc" [@@noalloc]

external get_cont_callstack :
  ('a, 'b) cont -> int -> Printexc.raw_backtrace =
  "caml_get_continuation_callstack"

type last_fiber

type 'b effc =
  { effc : 'o 'e. ('o, 'e) perform -> ('o, 'b) cont -> last_fiber -> 'b }
  [@@unboxed][@@warning "-69"]

external alloc_stack :
  ('a -> 'b) ->
  (exn -> 'b) ->
  'b effc ->
  ('a, 'b) stack = "caml_alloc_stack"

type (+'a, 'es) r =
  | Val : 'a -> ('a, 'es) r
  | Exn : exn -> ('a, 'es) r
  | Op :
      ('o, 'e) op
      * ('e, 'es) Raw_handler.t
      * ('o, ('a, 'es) r) cont
      * last_fiber -> ('a, 'es) r

type 'a dummy

let dummy_op : 'a. unit -> ('a, 'a dummy) op = fun () -> Obj.magic ()
let dummy_handler : 'a. unit -> 'a dummy Handler.t = fun () ->
  Handler.of_raw (Raw_handler.weaken Raw_handler.zero)
let dummy_perform : 'a. unit -> ('a, 'a dummy) perform = fun () ->
  dummy_op (), dummy_handler ()

let valuec v = Val v
let exnc e = Exn e

let alloc_cont
    (type a h b es) (f : (*local_*) h -> a -> b) (h : h) : (a, (b, es) r) cont =
  let exception Ready__ of (a, (b, es) r) cont in
  let effc (type o e) ((op, h) : (o, e) perform)
      (k : (o, (b, es) r) cont) last_fiber =
    let h = Handler.unsafe_to_raw h in
    if Raw_handler.is_invalid h then begin
      let k = (Obj.magic k : (a, (b, es) r) cont) in
      raise_notrace (Ready__ k)
    end else begin
      Op(op, h, k, last_fiber)
    end
  in
  let dummy = dummy_perform () in
  let s = alloc_stack valuec exnc {effc} in
  match runstack s (fun () -> f h (perform dummy)) () with
  | _ -> assert false
  | exception Ready__ k -> k

let run_stack
    (type h a es) (f : (*local_*) h -> a) (h : h) : (a, es) r =
  let effc (op, h) k last_fiber =
    let h = Handler.unsafe_to_raw h in
    Op(op, h, k, last_fiber)
  in
  let s = alloc_stack valuec exnc {effc} in
  runstack s (fun h -> f h) h

type (-'a, +'b, 'e, 'es) continuation =
  Cont :
    { cont : ('a, ('b, 'e * 'es) r) cont;
      mapping : ('es, 'eso) Mapping.t; }
    -> ('a, 'b, 'e, 'es) continuation

type ('a, 'e, 'es) res =
  | Value : 'a -> ('a, 'e, 'es) res
  | Exception : exn -> ('a, 'e, 'es) res
  | Operation :
      ('o, 'e) op * ('o, 'a, 'e, 'es) continuation
      -> ('a, 'e, 'es) res

let get_callstack (Cont { cont; _ }) i =
  get_cont_callstack cont i

external reperform :
  ('a, 'e) perform -> ('a, 'b) cont -> last_fiber -> 'b = "%reperform"

let rec handle :
  type a e es1 es2. (es1, es2) Mapping.t -> (a, e * es1) r -> (a, e, es1) res =
  fun mapping -> function
    | Val x -> Value x
    | Exn e -> Exception e
    | Op(op, handler, k, last_fiber) -> begin
        match Raw_handler.is_zero handler with
        | Some Refl -> Operation(op, Cont { cont = k; mapping })
        | None ->
            let handler = Raw_handler.weaken handler in
            let fwd = Mapping.lookup handler mapping in
            let fwd = Handler.of_raw fwd in
            handle mapping (reperform (op, fwd) k last_fiber)
      end

let resume (Cont { cont; mapping }) f x ((*local_*) handlers) =
  let handlers = Handlers.unsafe_to_raw handlers in
  Mapping.set handlers mapping;
  handle mapping (resume (take_cont_noexc cont) f x)

let continue k v ((*local_*) hs) = resume k (fun x -> x) v hs

let discontinue k e ((*local_*) hs) = resume k (fun e -> raise e) e hs

let discontinue_with_backtrace k e bt ((*local_*) hs) =
  resume k (fun e -> Printexc.raise_with_backtrace e bt) e hs

let fiber (type a b e)
    (f : (*local_*) e Handler.t -> a -> b) =
  let handler : (e, e * unit) Raw_handler.t = Raw_handler.zero in
  let handler : e Handler.t = Handler.of_raw handler in
  let mapping = Mapping.empty in
  let cont = alloc_cont f handler in
  Cont { cont; mapping }

let fiber_with (type a b e es) ((*local_*) l : es Length.t)
    (f : (*local_*) (e * es) Handlers.t -> a -> b) =
  let handler : (e, e * es) Raw_handler.t = Raw_handler.zero in
  let handlers : (e * es, e * es) Raw_handlers.t =
    handler :: Raw_handlers.initial ~length:l
  in
  let handlers = Handlers.of_raw handlers in
  let mapping = Mapping.create_unset l in
  let cont = alloc_cont f handlers in
  Cont { cont; mapping }

let run (type a e) (f : (*local_*) e Handler.t -> a) =
  let handler : (e, e * unit) Raw_handler.t = Raw_handler.zero in
  let handler : e Handler.t = Handler.of_raw handler in
  let res = run_stack f handler in
  handle Mapping.empty res

let run_with (type a e es) ((*local_*) hs : es Handlers.t)
    (f : (*local_*) (e * es) Handlers.t -> a) =
  let hs = Handlers.unsafe_to_raw hs in
  let handler : (e, e * es) Raw_handler.t = Raw_handler.zero in
  let handlers : (e * es, e * es) Raw_handlers.t =
    handler :: Raw_handlers.initial_from hs
  in
  let handlers = Handlers.of_raw handlers in
  let mapping = Mapping.create hs in
  let res = run_stack f handlers in
  handle mapping res

module Continuation = struct

  type (-'a, +'b, 'es) t =
    Continuation : ('a, 'c, 'e, 'es) continuation -> ('a, 'b, 'es) t
  [@@unboxed]
  (* This type has an unexpressible constraint that ['b] is a type that
     can safely be [Obj.magic]ed from [(c, e, es) res] *)

  let get_callstack (Continuation cont) i =
    get_callstack cont i
end

let continue (type a b es)
    (k : (a, b, es) Continuation.t) v ((*local_*) hs) =
  let Continuation (type e c) (cont : (a, c, e, es) continuation) = k in
  let res : (c, e, es) res = continue cont v hs in
  (Obj.magic res : b)

let discontinue (type a b es)
    (k : (a, b, es) Continuation.t) e ((*local_*) hs) =
  let Continuation (type e c) (cont : (a, c, e, es) continuation) = k in
  let res : (c, e, es) res = discontinue cont e hs in
  (Obj.magic res : b)

let discontinue_with_backtrace (type a b es)
    (k : (a, b, es) Continuation.t) e bt ((*local_*) hs) =
  let Continuation (type e c) (cont : (a, c, e, es) continuation) = k in
  let res : (c, e, es) res = discontinue_with_backtrace cont e bt hs in
  (Obj.magic res : b)

module type S = sig

  type ('o, 'e) ops

  type t

  module Result : sig

    type eff := t

    type ('a, 'es) t =
      | Value : 'a -> ('a, 'es) t
      | Exception : exn -> ('a, 'es) t
      | Operation :
          ('o, eff) ops * ('o, ('a, 'es) t, 'es) Continuation.t
          -> ('a, 'es) t

    type ('a, 'es) handler =
      { handle :
          'o. ('o, eff) ops
          -> ('o, ('a, 'es) t, 'es) Continuation.t
          -> 'a }
      [@@unboxed]

    val handle : ('a, 'es) t -> ('a, 'es) handler -> 'a

  end

  type ('a, 'es) result = ('a, 'es) Result.t =
    | Value : 'a -> ('a, 'es) result
    | Exception : exn -> ('a, 'es) result
    | Operation :
        ('o, t) ops * ('o, ('a, 'es) result, 'es) Continuation.t
        -> ('a, 'es) result

  val fiber :
    ((*local_*) t Handler.t -> 'a -> 'b)
    -> ('a, ('b, unit) Result.t, unit) Continuation.t

  val fiber_with :
    (*local_*) 'es Handlers.Length.t
    -> ((*local_*) (t * 'es) Handlers.t -> 'a -> 'b)
    -> ('a, ('b, 'es) Result.t, 'es) Continuation.t

  val run : ((*local_*) t Handler.t -> 'a) -> ('a, unit) Result.t

  val run_with :
    (*local_*) 'es Handlers.t
    -> ((*local_*) (t * 'es) Handlers.t -> 'a)
    -> ('a, 'es) Result.t

  val perform : (*local_*) t Handler.t -> ('a, t) ops -> 'a

  module Handler : sig

    type nonrec t = t Handler.t

  end

  module Continuation : sig

    type ('a, 'b, 'es) t =
      ('a, ('b, 'es) Result.t, 'es) Continuation.t

  end

end

module type S1 = sig

  type ('o, 'p, 'e) ops

  type 'p t

  module Result : sig

    type 'p eff := 'p t

    type ('a, 'p, 'es) t =
      | Value : 'a -> ('a, 'p, 'es) t
      | Exception : exn -> ('a, 'p, 'es) t
      | Operation :
          ('o, 'p, 'p eff) ops * ('o, ('a, 'p, 'es) t, 'es) Continuation.t
          -> ('a, 'p, 'es) t

    type ('a, 'p, 'es) handler =
      { handle :
          'o. ('o, 'p, 'p eff) ops
          -> ('o, ('a, 'p, 'es) t, 'es) Continuation.t
          -> 'a }
      [@@unboxed]

    val handle : ('a, 'p, 'es) t -> ('a, 'p, 'es) handler -> 'a

  end

  type ('a, 'p, 'es) result = ('a, 'p, 'es) Result.t =
    | Value : 'a -> ('a, 'p, 'es) result
    | Exception : exn -> ('a, 'p, 'es) result
    | Operation :
        ('o, 'p, 'p t) ops * ('o, ('a, 'p, 'es) result, 'es) Continuation.t
        -> ('a, 'p, 'es) result

  val fiber :
    ((*local_*) 'p t Handler.t -> 'a -> 'b)
    -> ('a, ('b, 'p, unit) Result.t, unit) Continuation.t

  val fiber_with :
    (*local_*) 'es Handlers.Length.t
    -> ((*local_*) ('p t * 'es) Handlers.t -> 'a -> 'b)
    -> ('a, ('b, 'p, 'es) Result.t, 'es) Continuation.t

  val run : ((*local_*) 'p t Handler.t -> 'a) -> ('a, 'p, unit) Result.t

  val run_with :
    (*local_*) 'es Handlers.t
    -> ((*local_*) ('p t * 'es) Handlers.t -> 'a)
    -> ('a, 'p, 'es) Result.t

  val perform : (*local_*) 'p t Handler.t -> ('a, 'p, 'p t) ops -> 'a

  module Handler : sig

    type nonrec 'p t = 'p t Handler.t

  end

  module Continuation : sig

    type ('a, 'b, 'p, 'es) t =
      ('a, ('b, 'p, 'es) Result.t, 'es) Continuation.t

  end

end

module type S2 = sig

  type ('o, 'p, 'q, 'e) ops

  type ('p, 'q) t

  module Result : sig

    type ('p, 'q) eff := ('p, 'q) t

    type ('a, 'p, 'q, 'es) t =
      | Value : 'a -> ('a, 'p, 'q, 'es) t
      | Exception : exn -> ('a, 'p, 'q, 'es) t
      | Operation :
          ('o, 'p, 'q, ('p, 'q) eff) ops
          * ('o, ('a, 'p, 'q, 'es) t, 'es) Continuation.t
          -> ('a, 'p, 'q, 'es) t

    type ('a, 'p, 'q, 'es) handler =
      { handle :
          'o. ('o, 'p, 'q, ('p, 'q) eff) ops
          -> ('o, ('a, 'p, 'q, 'es) t, 'es) Continuation.t
          -> 'a }
      [@@unboxed]

    val handle : ('a, 'p, 'q, 'es) t -> ('a, 'p, 'q, 'es) handler -> 'a

  end

  type ('a, 'p, 'q, 'es) result = ('a, 'p, 'q, 'es) Result.t =
    | Value : 'a -> ('a, 'p, 'q, 'es) result
    | Exception : exn -> ('a, 'p, 'q, 'es) result
    | Operation :
        ('o, 'p, 'q, ('p, 'q) t) ops
        * ('o, ('a, 'p, 'q, 'es) result, 'es) Continuation.t
        -> ('a, 'p, 'q, 'es) result

  val fiber :
    ((*local_*) ('p, 'q) t Handler.t -> 'a -> 'b)
    -> ('a, ('b, 'p, 'q, unit) result, unit) Continuation.t

  val fiber_with :
    (*local_*) 'es Handlers.Length.t
    -> ((*local_*) (('p, 'q) t * 'es) Handlers.t -> 'a -> 'b)
    -> ('a, ('b, 'p, 'q, 'es) result, 'es) Continuation.t

  val run :
    ((*local_*) ('p, 'q) t Handler.t -> 'a)
    -> ('a, 'p, 'q, unit) result

  val run_with :
    (*local_*) 'es Handlers.t
    -> ((*local_*) (('p, 'q) t * 'es) Handlers.t -> 'a)
    -> ('a, 'p, 'q, 'es) result

  val perform :
    (*local_*) ('p, 'q) t Handler.t
    -> ('a, 'p, 'q, ('p, 'q) t) ops
    -> 'a

  module Handler : sig

    type nonrec ('p, 'q) t = ('p, 'q) t Handler.t

  end

  module Continuation : sig

    type ('a, 'b, 'p, 'q, 'es) t =
      ('a, ('b, 'p, 'q, 'es) result, 'es) Continuation.t

  end

end

module type Operations = sig

  type 'a t

end

module type Operations_rec = sig

  type ('a, 'e) t

end

module type Operations1 = sig

  type ('a, 'p) t

end

module type Operations1_rec = sig

  type ('a, 'p, 'e) t

end

module type Operations2 = sig

  type ('a, 'p, 'q) t

end

module type Operations2_rec = sig

  type ('a, 'p, 'q, 'e) t

end

module Make_rec (Ops : Operations_rec)
  : S with type ('a, 'e) ops := ('a, 'e) Ops.t
= struct

  type t

  module Result = struct

    type eff = t

    type ('e, 'es) t =
    | Value : 'a -> ('a, 'es) t
    | Exception : exn -> ('a, 'es) t
    | Operation :
        ('o, eff) Ops.t * ('o, ('a, 'es) t, 'es) Continuation.t
        -> ('a, 'es) t

    type ('a, 'es) handler =
      { handle : 'o. ('o, eff) Ops.t -> ('o, ('a, 'es) t, 'es) Continuation.t -> 'a }
      [@@unboxed]

    let handle r {handle} =
      match r with
      | Value x -> x
      | Exception e -> raise e
      | Operation(op, k) -> handle op k

  end

  type ('a, 'es) result = ('a, 'es) Result.t =
    | Value : 'a -> ('a, 'es) result
    | Exception : exn -> ('a, 'es) result
    | Operation :
        ('o, t) Ops.t * ('o, ('a, 'es) result, 'es) Continuation.t
        -> ('a, 'es) result

  let fiber (type a b) f =
    let k : (a, b, t, unit) continuation = fiber f in
    (Continuation k : (a, (b, unit) Result.t, unit) Continuation.t)

  let fiber_with (type a b es) ((*local_*) hs) f =
    let k : (a, b, t, es) continuation = fiber_with hs f in
    (Continuation k : (a, (b, es) Result.t, es) Continuation.t)

  let run (type a) f =
    let res : (a, t, unit) res = run f in
    (Obj.magic res : (a, unit) Result.t)

  let run_with (type a es) ((*local_*) hs) f =
    let res : (a, t, es) res = run_with hs f in
    (Obj.magic res : (a, es) Result.t)

  let perform (type a) h (op : (a, t) Ops.t) =
    let op : (a, t) op = Obj.magic op in
    perform (op, h)

  module Handler = struct

    type nonrec t = t Handler.t

  end

  module Continuation = struct

    type ('a, 'b, 'es) t =
      ('a, ('b, 'es) Result.t, 'es) Continuation.t

  end

end

module Make (Ops : Operations)
  : S with type ('a, 'e) ops := 'a Ops.t
  = Make_rec(struct type ('a, 'e) t = 'a Ops.t end)

module Make1_rec (Ops : Operations1_rec)
  : S1 with type ('a, 'p, 'e) ops := ('a, 'p, 'e) Ops.t
= struct

  type 'p t

  module Result = struct

    type 'p eff = 'p t

    type ('a, 'p, 'es) t =
      | Value : 'a -> ('a, 'p, 'es) t
      | Exception : exn -> ('a, 'p, 'es) t
      | Operation :
          ('o, 'p, 'p eff) Ops.t * ('o, ('a, 'p, 'es) t, 'es) Continuation.t
          -> ('a, 'p, 'es) t

    type ('a, 'p, 'es) handler =
      { handle :
          'o. ('o, 'p, 'p eff) Ops.t
          -> ('o, ('a, 'p, 'es) t, 'es) Continuation.t
          -> 'a }
      [@@unboxed]

    let handle r {handle} =
      match r with
      | Value x -> x
      | Exception e -> raise e
      | Operation(op, k) -> handle op k

  end

  type ('a, 'p, 'es) result = ('a, 'p, 'es) Result.t =
    | Value : 'a -> ('a, 'p, 'es) result
    | Exception : exn -> ('a, 'p, 'es) result
    | Operation :
        ('o, 'p, 'p t) Ops.t * ('o, ('a, 'p, 'es) result, 'es) Continuation.t
        -> ('a, 'p, 'es) result

  let fiber (type a b p) f =
    let k : (a, b, p t, unit) continuation = fiber f in
    (Continuation k : (a, (b, p, unit) Result.t, unit) Continuation.t)

  let fiber_with (type a b p es) ((*local_*) hs) f =
    let k : (a, b, p t, es) continuation = fiber_with hs f in
    (Continuation k : (a, (b, p, es) Result.t, es) Continuation.t)

  let run_with (type a p es) ((*local_*) hs) f =
    let res : (a, p t, es) res = run_with hs f in
    (Obj.magic res : (a, p, es) Result.t)

  let run (type a p) f =
    let res : (a, p t, unit) res = run f in
    (Obj.magic res : (a, p, unit) Result.t)

  let perform (type a p) h (op : (a, p, p t) Ops.t) =
    let op : (a, p t) op = Obj.magic op in
    perform (op, h)

  module Handler = struct

    type nonrec 'p t = 'p t Handler.t

  end

  module Continuation = struct

    type ('a, 'b, 'p, 'es) t =
      ('a, ('b, 'p, 'es) Result.t, 'es) Continuation.t

  end

end

module Make1 (Ops : Operations1)
  : S1 with type ('a, 'p, 'e) ops := ('a, 'p) Ops.t
  = Make1_rec(struct type ('a, 'p, 'e) t = ('a, 'p) Ops.t end)

module Make2_rec (Ops : Operations2_rec)
  : S2 with type ('a, 'p, 'q, 'e) ops := ('a, 'p, 'q, 'e) Ops.t
= struct

  type ('p, 'q) t

  module Result = struct

    type ('p, 'q) eff = ('p, 'q) t

    type ('a, 'p, 'q, 'es) t =
      | Value : 'a -> ('a, 'p, 'q, 'es) t
      | Exception : exn -> ('a, 'p, 'q, 'es) t
      | Operation :
          ('o, 'p, 'q, ('p, 'q) eff) Ops.t
          * ('o, ('a, 'p, 'q, 'es) t, 'es) Continuation.t
          -> ('a, 'p, 'q, 'es) t

    type ('a, 'p, 'q, 'es) handler =
      { handle :
          'o. ('o, 'p, 'q, ('p, 'q) eff) Ops.t
          -> ('o, ('a, 'p, 'q, 'es) t, 'es) Continuation.t
          -> 'a }
      [@@unboxed]

    let handle r {handle} =
      match r with
      | Value x -> x
      | Exception e -> raise e
      | Operation(op, k) -> handle op k

  end

  type ('a, 'p, 'q, 'es) result = ('a, 'p, 'q, 'es) Result.t =
    | Value : 'a -> ('a, 'p, 'q, 'es) result
    | Exception : exn -> ('a, 'p, 'q, 'es) result
    | Operation :
        ('o, 'p, 'q, ('p, 'q) t) Ops.t
        * ('o, ('a, 'p, 'q, 'es) result, 'es) Continuation.t
        -> ('a, 'p, 'q, 'es) result

  let fiber (type a p q b) f =
    let k : (a, b, (p, q) t, unit) continuation = fiber f in
    (Continuation k : (a, (b, p, q, unit) result, unit) Continuation.t)

  let fiber_with (type a p q b es) ((*local_*) hs) f =
    let k : (a, b, (p, q) t, es) continuation = fiber_with hs f in
    (Continuation k : (a, (b, p, q, es) result, es) Continuation.t)

  let run (type a p q) f =
    let res : (a, (p, q) t, unit) res = run f in
    (Obj.magic res : (a, p, q, unit) result)

  let run_with (type a p q es) ((*local_*) hs) f =
    let res : (a, (p, q) t, es) res = run_with hs f in
    (Obj.magic res : (a, p, q, es) result)

  let perform (type a p q) h (op : (a, p, q, (p, q) t) Ops.t) =
    let op : (a, (p, q) t) op = Obj.magic op in
    perform (op, h)

  module Handler = struct

    type nonrec ('p, 'q) t = ('p, 'q) t Handler.t

  end

  module Continuation = struct

    type ('a, 'b, 'p, 'q, 'es) t =
      ('a, ('b, 'p, 'q, 'es) result, 'es) Continuation.t

  end

end

module Make2 (Ops : Operations2)
  : S2 with type ('a, 'p, 'q, 'e) ops := ('a, 'p, 'q) Ops.t
  = Make2_rec(struct type ('a, 'p, 'q, 'e) t = ('a, 'p, 'q) Ops.t end)

exception Continuation_already_resumed

(* Register the exception so that the runtime can access it *)
let _ = Callback.register_exception "Effect.Continuation_already_resumed"
          Continuation_already_resumed
