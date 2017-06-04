open Core.Std
open Lexing

open Parser
open Lexer
open Ast
open Util
open TypeChecker

exception RuntimeError of string
(*
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
*)

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
    Util.print_ast ast;
    Util.print_ty (TypeChecker.check ast)
