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

let rec print_term =
    fun tm:Ast.term ->
    match tm with
    | Num_term num -> print_int num
    | Add_term  (tm1,tm2) -> print_tm tm1; print_string "+"; print_tm tm2 
    | Sub_term  (tm1,tm2) -> print_tm tm1; print_string "-"; print_tm tm2 
    | Le_term   (tm1,tm2) -> print_tm tm1; print_string "<"; print_tm tm2 
    | Eq_term   (tm1,tm2) -> print_tm tm1; print_string "=="; print_tm tm2 
    | Bool_term b -> if b then print_string "true" else print_string "false"
    | And_term  (tm1,tm2) -> print_tm tm1; print_string "&"; print_tm tm2 
    | Or_term   (tm1,tm2) -> print_tm tm1; print_string "||"; print_tm tm2 
    | Not_term  -> print_string "~"

    | Var_term       of string
    | LetUn_term     of string * term * term
    | LetLin_term    of string * term * term
    | Letrec_term    of string * ty * term * term
    | If_term        of term * term * term
    | If_null_term   of string * term * term
    | Lambda_term    of string * ty * term
    | App_term       of string * term
    | Begin_term     of term list

    | NewLinRes_term        of string
    | CopyAtom_term         of term * string * string
    | CopyList_term         of term * string * string
    | FreeAtom_term         of term
    | FreeList_term         of term
    | Print_term            of term
    | LinCons_term      of term * term
    | Split_term        of term * string * string
    | Null_term


let print_ast =
    fun ast:Ast.program ->
    match ast with
    | A_program tm -> print_term tm



let _ = print_ast ast; print_string "\n"
