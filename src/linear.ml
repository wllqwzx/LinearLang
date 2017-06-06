open Core.Std
open Lexing

open Parser
open Lexer
open Ast
open Util
open TypeChecker
open Runtime

exception RuntimeError of string

let getAst = 
    fun str ->
    let alexbuf = Lexing.from_string str in
    try
    Parser.enterPoint Lexer.read alexbuf
    with
    | Lexer.LexerError str -> print_string str; exit 1
    | Parser.Error -> print_string ("!!! Parser error with char: " ^ (Lexing.lexeme alexbuf)
                                        ^ " at: " ^ (Lexer.error_info alexbuf)); exit 1



(*-------------------------------------------------------*)


let fileName = ref ""

let () =
    fileName := Sys.argv.(1);
    let file = In_channel.create !fileName in
    let str = In_channel.input_all file in
    In_channel.close file;
    let ast = getAst str in
    try
        (*Util.print_ast ast;*)
        print_string "type checking-------------------------------\n";
        let typ  = (TypeChecker.check ast) in
        print_string "type: "; Util.print_ty typ;
        print_string "\n\nevaluation----------------------------------\n";
        let v = (Runtime.value_of_program ast) in
        print_string "\nresult: "; Runtime.print_val v;
        print_newline ();
    with  TypeError str -> print_string ("!!!Type Error: " ^ str); print_newline ()
        | RuntimeError str -> print_string ("!!!Runtime Error: " ^ str); print_newline ()
    
    