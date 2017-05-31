open Core.Std
open Lexing

open Parser
open Lexer
open Ast

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

let rec print_ty =
    fun (typ:Ast.ty) ->
    match typ with
    | NumTy         -> print_string "Num"
    | BoolTy        -> print_string "Bool"
    | LinResTy      -> print_string "LinRes"
    | LinListTy     -> print_string "LinList"
    | UnitTy        -> print_string "Unit"
    | ArrowTy (ty1,ty2) -> print_ty ty1; print_string "->"; print_ty ty2


let rec print_term =
    fun (tm:Ast.term) ->
   (match tm with
    | Num_term  num -> print_int num
    | Add_term  (tm1,tm2) -> print_term tm1; print_string "+"; print_term tm2 
    | Sub_term  (tm1,tm2) -> print_term tm1; print_string "-"; print_term tm2 
    | Le_term   (tm1,tm2) -> print_term tm1; print_string "<"; print_term tm2 
    | Eq_term   (tm1,tm2) -> print_term tm1; print_string "=="; print_term tm2 
    | Bool_term b -> if b then print_string "true" else print_string "false"
    | And_term  (tm1,tm2) -> print_term tm1; print_string "&"; print_term tm2 
    | Or_term   (tm1,tm2) -> print_term tm1; print_string "||"; print_term tm2 
    | Not_term  tm        -> print_string "~ "; print_term tm

    | Var_term      str -> print_string str
    | LetUn_term    (str,tm1,tm2) -> print_string ("LetUn " ^ str ^ " = "); print_term tm1; print_string ";\n"; print_term tm2
    | LetLin_term   (str,tm1,tm2) -> print_string ("LetLin " ^ str ^ " = "); print_term tm1; print_string ";\n"; print_term tm2
    | Letrec_term   (str,typ,tm1,tm2) -> print_string ("Letrec " ^ str ^ " : "); print_ty typ; print_string " = "; print_term tm1; print_string ";\n"; print_term tm2
    | If_term       (tm1,tm2,tm3) -> print_string "if "; print_term tm1; print_string "\nthen "; print_term tm2; print_string "\nelse "; print_term tm3; print_newline ()
    | If_null_term  (str,tm1,tm2) -> print_string ("if-null " ^ str ^ "\nthen "); print_term tm1; print_string "\nelse "; print_term tm2; print_newline ()
    | Lambda_term   (str,typ,tm) -> print_string ("fun " ^ str ^ " : "); print_ty typ; print_string " {\n"; print_term tm; print_string "}\n"
    | App_term      (str,tm) -> print_string (str ^ "("); print_term tm; print_string ")"
    | Begin_term    tmli -> print_string "begin\n"; print_tmli tmli; print_string "end\n"

    | NewLinRes_term    str -> print_string ("newRes(" ^ str ^ ")")
    | CopyAtom_term     (tm,str1,str2) -> print_string "copyAtom "; print_term tm; print_string (" as " ^ str1 ^ " and " ^ str2 ^ ";\n")
    | CopyList_term     (tm,str1,str2) -> print_string "copyList "; print_term tm; print_string (" as " ^ str1 ^ " and " ^ str2 ^ ";\n")
    | FreeAtom_term      tm -> print_string "freeAtom("; print_term tm; print_string ")\n"
    | FreeList_term      tm -> print_string "freeList("; print_term tm; print_string ")\n"
    | Print_term         tm -> print_string "print("; print_term tm; print_string ")\n"
    | LinCons_term      (tm1,tm2) -> print_string "LinCons("; print_term tm1; print_string ", "; print_term tm2; print_string ")\n"
    | Split_term        (tm,str1,str2) -> print_string "split "; print_term tm; print_string (" as " ^ str1 ^ " and " ^ str2 ^ ";\n")
    | Null_term          -> print_string "null")

and print_tmli =
    fun tmli ->
    match tmli with
    | [] -> ()
    | tm::tms -> print_term tm; print_tmli tms

let print_ast =
    fun (ast:Ast.program) ->
    match ast with
    | A_program tm -> print_term tm



let fileName = ref ""

let () =
    fileName := Sys.argv.(1);
    let file = In_channel.create !fileName in
    let str = In_channel.input_all file in
    In_channel.close file;
    let ast = getAst str in
    print_ast ast
