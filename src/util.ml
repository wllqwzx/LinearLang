open Core.Std
open Lexing

open Parser
open Lexer
open Ast

let rec print_ty =
    fun (typ:Ast.ty) ->
    match typ with
    | NumTy         -> print_string "NumTy"
    | BoolTy        -> print_string "BoolTy"
    | LinResTy      -> print_string "LinResTy"
    | LinListTy     -> print_string "LinListTy"
    | UnitTy        -> print_string "Unit"
    | ArrowTy (ty1,ty2) -> print_ty ty1; print_string "->"; print_ty ty2


let rec print_term =
    fun (tm:Ast.term) ->
   (match tm with
    | Num_term  num -> print_int num
    | Add_term  (tm1,tm2) -> print_term tm1; print_string "+"; print_term tm2 
    | Sub_term  (tm1,tm2) -> print_term tm1; print_string "-"; print_term tm2 
    | Mul_term  (tm1,tm2) -> print_term tm1; print_string "*"; print_term tm2 
    | Le_term   (tm1,tm2) -> print_term tm1; print_string "<"; print_term tm2 
    | Eq_term   (tm1,tm2) -> print_term tm1; print_string "=="; print_term tm2 
    | Bool_term b -> if b then print_string "true" else print_string "false"
    | And_term  (tm1,tm2) -> print_term tm1; print_string "&"; print_term tm2 
    | Or_term   (tm1,tm2) -> print_term tm1; print_string "||"; print_term tm2 
    | Not_term  tm        -> print_string "~ "; print_term tm

    | Var_term      str -> print_string str
    | LetUn_term    (str,tm1,tm2) -> print_string ("LetUn " ^ str ^ " = "); print_term tm1; print_string ";\n"; print_term tm2
    | LetLin_term   (str,tm1,tm2) -> print_string ("LetLin " ^ str ^ " = "); print_term tm1; print_string ";\n"; print_term tm2
    (*| Letrec_term   (str,typ,tm1,tm2) -> print_string ("Letrec " ^ str ^ " : "); print_ty typ; print_string " = "; print_term tm1; print_string ";\n"; print_term tm2*)
    | Fix_term      tm -> print_string ("fix "); print_term tm
    | If_term       (tm1,tm2,tm3) -> print_string "if "; print_term tm1; print_string "\nthen "; print_term tm2; print_string "\nelse "; print_term tm3; print_newline ()
    | If_null_term  (str,tm1,tm2) -> print_string ("if-null " ^ str ^ "\nthen "); print_term tm1; print_string "\nelse "; print_term tm2; print_newline ()
    | Lambda_term   (str,typ,tm) -> print_string ("fun " ^ str ^ " : "); print_ty typ; print_string " {\n"; print_term tm; print_string "}\n"
    | App_term      (str,tm) -> print_string (str ^ "("); print_term tm; print_string ")"
    | Begin_term    tmli -> print_string "begin\n"; print_tmli tmli; print_string "end\n"

    | NewLinRes_term    str -> print_string ("newRes(" ^ str ^ ")")
    | CopyAtom_term     (tm,str1,str2,ast3) -> print_string "copyAtom "; print_term tm; print_string (" as " ^ str1 ^ " and " ^ str2 ^ "\n"); print_term ast3
    | CopyList_term     (tm,str1,str2,ast3) -> print_string "copyList "; print_term tm; print_string (" as " ^ str1 ^ " and " ^ str2 ^ "\n"); print_term ast3
    | FreeAtom_term      tm -> print_string "freeAtom("; print_term tm; print_string ")\n"
    | FreeList_term      tm -> print_string "freeList("; print_term tm; print_string ")\n"
    | Print_term         tm -> print_string "print("; print_term tm; print_string ")\n"
    | LinCons_term      (tm1,tm2) -> print_string "LinCons("; print_term tm1; print_string ", "; print_term tm2; print_string ")\n"
    | Split_term        (tm,str1,str2,ast3) -> print_string "split "; print_term tm; print_string (" as " ^ str1 ^ " and " ^ str2 ^ "\n"); print_term ast3
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
