module Main

open FStar.Tactics
open FStar.Tactics.Builtins
module S = FStar.String
module L = FStar.List.Tot

let read_input () = launch_process "bash" ["-c";"cat /tmp/fifo_in"] ""
let write_output (s: string)
  = let _ = launch_process "bash" ["-c";"echo \""^s^"\" >> /tmp/fifo_out"] "" in
    ()

let string_of_unit (x: unit): string = "()"
let string_of_list (p: 'a -> string) (x: list 'a): string = "[" ^ S.concat ";" (L.map p x) ^ "]"
let either_to_string (a:'a -> string) (b:'b -> string) (x: either 'a 'b): string
  = match x with
  | Inl v -> a v
  | Inr e -> "Err: " ^ b e
let string_of_tuple f g (x, y) = "(" ^ f x ^ ", " ^ g x ^  ")"

let trim_endline s = S.concat "\n" (match S.split ['\n'] s with
                                   | [] -> []
                                   | r -> L.init r)

let compatible_types_base: list (t: Type & (t -> string)) =
  [ (| string, (fun s -> s) |)
  ; (| int, string_of_int |)
  ; (| unit, string_of_unit |)
  ; (| bool, string_of_bool |)
  ; (| term, term_to_string |)
  ; (| binder, binder_to_string |)
  ; (| binder * binder, string_of_tuple binder_to_string binder_to_string |)
  ]

let compatible_types_base_list: list (t: Type & (t -> string))
  = let h: (t: Type & (t -> string)) -> (t: Type & (t -> string))
        = fun (|t,f|) -> (|list t, string_of_list f|)
    in
    compatible_types_base
  @ L.map h compatible_types_base

let as_tot: (t: Type & (t -> string)) -> (t: Type & (t -> Tac string))
  = fun t -> let (|t, f|) = t in 
          let p: (unit -> t) -> Tac string = fun x -> f (x ()) in
          (|(unit -> t), p|)

let as_tac: (t: Type & (t -> string)) -> (t: Type & (t -> Tac string))
  = fun t -> let (|t, f|) = t in 
          let p: (unit -> Tac t) -> Tac string = fun x -> f (x ()) in
          (|(unit -> Tac t), p|)

let as_utac: (t: Type & (t -> string)) -> (t: Type & (t -> Tac string))
  = fun t -> let (|t, f|) = t in 
          let p: (unit -> (unit -> Tac t)) -> Tac string = fun x -> f (x () ()) in
          (|(unit -> (unit -> Tac t)), p|)

let compatible_types' 
  = L.map as_tot compatible_types_base_list
  @ L.map as_tac compatible_types_base_list
  @ L.map as_utac compatible_types_base_list

exception NotMatching
let rec or_elseL (#a:Type) (l: list (unit -> Tac a)): Tac a =
    match l with 
    | [] -> fail "or_elseL empty list"
    | [hd] -> hd ()
    | hd::tl -> ( try hd ()
                with | NotMatching -> or_elseL #a tl
                     | e -> raise e
              )

let unquote' (t: term) (tt: term) (tup: (typ:Type & (typ->Tac string)))
  : Tac (unit -> Tac string)
  = let (| typ, f |) = tup in
    if unify (quote typ) tt
    then fun () -> f (unquote #typ t)
    else fun () -> raise NotMatching

let rec interact (): Tac unit
  = dump "";
    let list_goals = goals () in
    if L.length list_goals = 0 
    then write_output "No more goal!" 
    else begin
    let s = trim_endline (read_input ()) in
    if s = "quit"
    then ()
    else
      let ws = "fun _ -> (" ^ s ^ ")" in
      let err msg #a (): Tac (option a) = write_output (msg); None in
      let run (): Tac _ =
        let t = string_to_term (top_env ()) ws in
        let tt = tc (top_env ()) t in
        or_elseL (map (unquote' t tt) compatible_types')
      in
      let on_error: _ -> Tac _ = function
        | NotMatching -> write_output "The term could not be unquote."
        | TacticFailure e -> write_output e
        | e -> raise e
      in
      (try
      ( match catch run with
      | Inr v -> write_output v
      | Inl e -> on_error e
      ) with
      | e -> on_error e
      );
      interact ()
    end

