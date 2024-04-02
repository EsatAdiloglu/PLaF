(* Environment Abstracted Result *)
type exp_val =
  | NumVal of int
  | BoolVal of bool
  
type 'a result = Ok of 'a | Error of string

type env = 
  | EmptyEnv
  | ExtendEnv of string*exp_val*env
  
let return : 'a -> 'a result =
  fun v ->
  Ok v

let error : string -> 'a result =
  fun s ->
  Error s

let (>>=) : 'a result -> ('a -> 'b result) -> 'b result =
  fun c f ->
  match c with
  | Error err -> Error err
  | Ok v -> f v


let e1 = ExtendEnv("x",NumVal 3, ExtendEnv("y", BoolVal false, EmptyEnv))

let rec apply_env id en =
  match en with
  | EmptyEnv -> Error("Unbound variable "^id)
  | ExtendEnv(id',ev,tl)->
    if id = id'
    then Ok ev
  else apply_env id tl