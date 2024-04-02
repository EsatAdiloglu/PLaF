open Ds
open Parser_plaf.Ast
open Parser_plaf.Parser

(* type exp_val =
  | NumVal of int
  | BoolVal of bool
  
type 'a result = Ok of 'a | Error of string *)

(** [eval_expr e] evaluates expression [e] *)
let rec eval_expr : expr -> exp_val result =
  fun e ->
  match e with
  | Int (n)     -> return (NumVal n)
  | Var(id) ->
    apply_env id en
  | Add(e1,e2) ->
    eval_expr e1 en >>= fun ev1 ->
    int_of_numVal ev1 >>= fun n1 ->
    eval_expr e2 en >>= fun ev2 ->
    int_of_numVal ev2 >>= fun n2 ->
    return (NumVal(n1+n2))  
  | Sub(e1,e2) ->
    eval_expr e1 en >>= fun ev1 ->
      int_of_numVal ev1 >>= fun n1 ->
      eval_expr e2 en >>= fun ev2 ->
      int_of_numVal ev2 >>= fun n2 ->
      return (NumVal(n1-n2))  
  | Mul(e1,e2) ->
    eval_expr e1 en >>= fun ev1 ->
      int_of_numVal ev1 >>= fun n1 ->
      eval_expr e2 en >>= fun ev2 ->
      int_of_numVal ev2 >>= fun n2 ->
      return (NumVal(n1*n2))   
  | Div(e1,e2) ->
    eval_expr e1 en >>= fun ev1 ->
      int_of_numVal ev1 >>= fun n1 ->
      eval_expr e2 en >>= fun ev2 ->
      int_of_numVal ev2 >>= fun n2 ->
    if n2 = 0
    then error "Division by zero"
    else return (NumVal (n1/n2))
  | Abs(e) ->
    eval_expr e1 en >>= fun ev1 ->
      int_of_numVal ev1 en >>= fun n1 ->
    return (Num_val (abs n1))
  | Min(e1,e2) ->
    eval_expr e1 en >>= fun ev1 ->
      int_of_numVal ev1 >>= fun n1 ->
      eval_expr e2 en >>= fun ev2 ->
      int_of_numVal ev2 >>= fun n2 ->
    return (NumVal (min n1 n2))
  | IsZero(e) ->
    eval_expr e >>=
  int_of_numVal >>= fun n ->
    return (BoolVal (n=0))
  | ITE(e1,e2,e3) ->
    eval_expr e1 en >>=
    bool_of_boolVal >>= fun b ->
    if b
    then eval_expr e2 en
    else eval_expr e3 en
  | Let(id,e1,e2) ->
    eval_expr e1 en >>= fun w ->
    eval_expr e2 (en plus id:w)
  | _ -> failwith "Not implemented yet!"

(** [eval_prog e] evaluates program [e] *)
let eval_prog (AProg(_,e)) =
  eval_expr e

(** [interp s] parses [s] and then evaluates it *)
let interp (e:string) =
  let c = e |> parse |> eval_prog
  in c






