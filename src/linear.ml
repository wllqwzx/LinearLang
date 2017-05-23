open Core.Std
open Lexing

open Parser
open Lexer
open Ast

exception RuntimeError of string

let ast =
    match In_channel.input_line stdin with (* read from terminal, we modify it to read from a file *)
    | None -> print_endline "\nGood bye."; exit 0
    | Some line -> let alexbuf = Lexing.from_string line in
                   try
                   Parser.enterPoint Lexer.read alexbuf
                   with
                   | Lexer.LexerError str -> print_string str; exit 1
                   | Parser.Error -> print_string ("Oops!!! parser error with char: " ^ (Lexing.lexeme alexbuf)
                                                     ^ " at: " ^ (Lexer.error_info alexbuf)); exit 1




(*-------------------------------------------------------*)
(* expressed value & denoted value *)
(* environment & store *)

type proc =
    | Procedure of string * expression * env

and reference = int

and expval =
    | Num_val   of int
    | Bool_val  of bool
    | Proc_val  of proc
    | Ref_val   of reference
    | Unit_val

and env =
    | Empty_env
    | Extend_env of string * expval * env;; (* there are mutual recursive in env and expval, so we define together*)


let init_env =
    Extend_env ("i", (Num_val 1),
        (Extend_env ("v", (Num_val 5),
            (Extend_env ("x", (Num_val 10),
                Empty_env)))))

(* string -> env -> expval *)
let rec apply_env =
    fun var envi ->
        match envi with
        | Empty_env -> raise (RuntimeError ("!!! Unbound variable: " ^ var ^ " in env."))
        | Extend_env (var0, val0, env0) -> if var0 = var
                                           then val0
                                           else (apply_env var env0)


(* store *)

let store : expval ref list ref = ref []

(* auxiliary function: *)
let rec length =
    fun st ->
    match st with
    | [] -> 0
    | x::xl -> 1 + (length xl)

let rec append =
    fun st v ->
    match st with
    | [] -> (ref v)::[]
    | x::xl -> x::(append xl v)

let car =
    fun li ->
    match li with
    | [] -> raise (RuntimeError ("!!!No such location in store."))
    | x::xl -> x

let cdr =
    fun li ->
    match li with
    | [] -> raise (RuntimeError "!!!No such location in store.")
    | x::xl -> xl


let rec ref_of_index =
  fun (rf:reference) st ->
    match rf with
    | 0 -> (car st)
    | _ -> ref_of_index (rf - 1) (cdr st)

(* expval -> reference *)
let apply_newref =
    fun v ->
        let len = length !store in
        store := append !store v;
        len

(* reference -> expval -> () *)
let apply_setref =
    fun rf v ->
    let t = ref_of_index rf !store in
    t := v


(* reference -> expval *)
let apply_deref =
    fun rf ->
    let t = ref_of_index rf !store in
    !t






(* Eval *)
(*-------------------------------------------------------*)

let rec value_of =
    fun (exp:Ast.expression) (envi:env):expval ->
    match exp with
    | Const_exp v -> Num_val v
    | Diff_exp (exp1, exp2) ->  let ev1 = (value_of exp1 envi) in
                                let ev2 = (value_of exp2 envi) in
                                begin
                                match (ev1, ev2) with
                                | (Num_val v1, Num_val v2) -> Num_val (v1 - v2)
                                | _ -> print_string "Type error!!!, in diff exp, args do not have type: Num_val"; exit 1
                                end
    | Is_zero_exp exp1 -> let v1 = (value_of exp1 envi) in
                          if v1 = (Bool_val true) then (Bool_val true) else (Bool_val false)
    | If_exp (exp1, exp2, exp3) ->  let v1 = (value_of exp1 envi) in
                                    begin
                                    match v1 with
                                    | Bool_val t -> if t = true then (value_of exp2 envi) else (value_of exp3 envi)
                                    | _ -> print_string "Type error!!!, in if expression, condition does not has type: Bool_val"; exit 1
                                    end
    | Var_exp var -> apply_env var envi
    | Let_exp (var, exp1, exp2) -> value_of exp2 (Extend_env (var, (value_of exp1 envi), envi))
    | Proc_exp (var, body) -> Proc_val (Procedure (var, body, envi))
    | Call_exp (exp1, exp2) ->  let v1 = (value_of exp1 envi) in
                                begin
                                match v1 with
                                | Proc_val (Procedure (var, body, env0)) ->
                                        let v = value_of exp2 envi in
                                        value_of body (Extend_env (var, v, env0))
                                | _ -> print_string "!!!Type error, in call exp, first exp does not have type: Proc_val!"; exit 1
                                end
    | Begin_exp explist -> let rec listvalue =
                            fun lis ->
                            match lis with
                            | [] -> print_string "expression list can never be empty! this should not happen!!!"; exit 1
                            | exp::[] -> value_of exp envi
                            | exp::exps -> let _ = (value_of exp envi) in
                                            (listvalue exps)
                            in (listvalue explist)
    | Newref_exp exp -> Ref_val (apply_newref (value_of exp envi))
    | Deref_exp exp ->  let rf = (value_of exp envi) in
                        begin
                        match rf with
                        | Ref_val v -> apply_deref v
                        | _ -> print_string "Type error!!!, the exp in deref does not have type Ref_val!"; exit 1
                        end
    | Setref_exp (exp1, exp2) ->  let rf = (value_of exp1 envi) in
                                  let v = (value_of exp2 envi) in
                                  begin
                                  match rf with
                                  | Ref_val rfv -> apply_setref rfv v; Unit_val
                                  | _ -> print_string "Type error!!!, the exp in setref should have type Ref_val"; exit 1
                                  end


let value_of_program =
  fun pgm ->
    match pgm with
    |A_program exp -> (value_of exp init_env)


let print_expval =
  fun expv ->
    match expv with
    | Num_val v -> print_int v
    | Bool_val v -> if v = true then print_string "true" else print_string "false"
    | Proc_val p -> print_string "<procedure>"
    | Ref_val rf -> print_string "<Reference>"
    | Unit_val -> print_string "Unit"

let _ = print_expval (value_of_program ast); print_string "\n"
