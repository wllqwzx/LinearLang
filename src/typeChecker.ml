open Lexer
open Parser
open Ast

exception TypeError of string

type tenvData =
    | Empty_tenv
    | Extend_tenv   of string * ty * tenvData


let tenv = ref Empty_tenv


let isLinear =
    fun typ ->
    match typ with
    | LinResTy -> true
    | LinListTy -> true
    | _ -> false


let rec delete_var =
    fun env str ->
    match env with
    | Empty_tenv -> Empty_tenv
    | Extend_tenv (str0,typ,env0) -> if (String.equal str str0) 
                                     then if isLinear(typ) 
                                          then raise (TypeError ("Linear Variable " ^ str ^ " was unused!"))
                                          else env0
                                     else Extend_tenv (str0,typ,(delete_var env0 str))



let rec type_of =
    fun (tm:term) : ty ->
    match tm with
    | Num_term       n -> NumTy
    | Add_term       (tm1,tm2) -> let ty1 = type_of tm1 in
                                  let ty2 = type_of tm2 in
                                  if ty1 = NumTy && ty2 = NumTy
                                  then NumTy
                                  else raise (TypeError "type error in Add!")
    | Sub_term       (tm1,tm2) -> let ty1 = type_of tm1 in
                                  let ty2 = type_of tm2 in
                                  if ty1 = NumTy && ty2 = NumTy
                                  then NumTy
                                  else raise (TypeError "type error in Sub!")
    | Mul_term       (tm1,tm2) -> let ty1 = type_of tm1 in
                                  let ty2 = type_of tm2 in
                                  if ty1 = NumTy && ty2 = NumTy
                                  then NumTy
                                  else raise (TypeError "type error in Mul!")
    | Le_term        (tm1,tm2) -> let ty1 = type_of tm1 in
                                  let ty2 = type_of tm2 in
                                  if ty1 = NumTy && ty2 = NumTy
                                  then BoolTy
                                  else raise (TypeError "type error in Le!")
    | Eq_term        (tm1,tm2) -> let ty1 = type_of tm1 in
                                  let ty2 = type_of tm2 in
                                  if ty1 = NumTy && ty2 = NumTy
                                  then BoolTy
                                  else raise (TypeError "type error in Eq!")
    | Bool_term      b -> BoolTy
    | And_term       (tm1,tm2) -> let ty1 = type_of tm1 in
                                  let ty2 = type_of tm2 in
                                  if ty1 = BoolTy && ty2 = BoolTy
                                  then BoolTy
                                  else raise (TypeError "type error in And!")
    | Or_term        (tm1,tm2) -> let ty1 = type_of tm1 in
                                  let ty2 = type_of tm2 in
                                  if ty1 = BoolTy && ty2 = BoolTy
                                  then BoolTy
                                  else raise (TypeError "type error in Or!")
    | Not_term       tm1 -> let ty1 = type_of tm1 in
                            if ty1 = BoolTy
                            then BoolTy
                            else raise (TypeError "type error in Or!")
(*--------------------*)
    | Var_term       of string
    | If_term        of term * term * term
    | If_null_term   of string * term * term
    | Lambda_term    of string * ty * term
    | App_term       of string * term
    | Begin_term     of term list
    | LetUn_term     of string * term * term
    | LetLin_term    of string * term * term
    | Letrec_term    of string * ty * term * term
    
(*--------------------*)
    | NewLinRes_term    of string
    | CopyAtom_term     of term * string * string * term
    | CopyList_term     of term * string * string * term
    | Split_term        of term * string * string * term
    | FreeAtom_term     of term
    | FreeList_term     of term
    | Print_term        of term
    | LinCons_term      of term * term
    | Null_term



let check =
    fun ast ->
    match ast with
    | A_program tm -> type_of tm tenv



