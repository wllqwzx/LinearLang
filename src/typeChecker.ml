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


(* delete out of scope *)
let rec delete_var = 
    fun env str ->
    match env with
    | Empty_tenv -> Empty_tenv
    | Extend_tenv (str0,typ,env0) -> if (String.equal str str0) 
                                     then if isLinear(typ) 
                                          then raise (TypeError ("Linear Variable " ^ str ^ " was unused!"))
                                          else env0
                                     else Extend_tenv (str0,typ,(delete_var env0 str))


let rec lookup_type =
    fun env str ->
    match env with
    | Empty_tenv -> raise (TypeError ("Variable: " ^ str ^" clould not be find. It may be used or not defined!"))
    | Extend_tenv (str0,typ,env0) -> if (String.equal str str0)
                                     then typ
                                     else lookup_type env0 str

let rec use_var =
    fun env str ->
    match env with
    | Empty_tenv -> Empty_tenv
    | Extend_tenv (str0,typ,env0) -> if (String.equal str str0)
                                     then (match typ with
                                           | LinResTy -> env0
                                           | LinListTy -> env0
                                           | _ -> Extend_tenv (str0,typ,env0))
                                     else Extend_tenv (str0,typ,(use_var env0 str))



let rec type_of =
    fun (tm:term) : ty ->
   (match tm with
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
    | Var_term       str -> let typ = lookup_type !tenv str in
                            begin 
                                tenv := use_var !tenv str;
                                typ
                            end
    | If_term        (tm1,tm2,tm3) -> 
    | If_null_term   (str1,tm1,tm2) -> 
    | Lambda_term    (str,typ,tm) -> 
    | App_term       (str,tm) -> 
    | Begin_term     tmli -> 
    | LetUn_term     (str,tm1,tm2) -> 
    | LetLin_term    (str,tm1,tm2) -> 
    | Letrec_term    (str,typ,tm1,tm2) -> 
    
(*--------------------*)
    | NewLinRes_term    str -> LinResTy
    | CopyAtom_term     (tm1,str1,str2,tm2) -> 
    | CopyList_term     (tm1,str1,str2,tm2) -> 
    | Split_term        (tm1,str1,str2,tm2) -> 
    | FreeAtom_term     tm -> let typ = type_of tm in
                              if typ = LinResTy 
                              then UnitTy
                              else raise (TypeError ("type of freeAtom is LinResTy -> UnitTy"))
    | FreeList_term     tm -> let typ = type_of tm in
                              if typ = LinListTy 
                              then UnitTy
                              else raise (TypeError ("type of freeList is LinListTy -> UnitTy"))
    | Print_term        tm -> type_of tm
    | LinCons_term      (tm1,tm2) -> let typ1 = type_of tm1 in
                                     let typ2 = type_of tm2 in
                                     if typ1 = LinResTy && typ2 = LinListTy
                                     then LinListTy
                                     else raise (TypeError ("type of LinCons is LinResTy * LinListTy -> LinListTy!"))
    | Null_term         -> LinListTy)


let check =
    fun ast ->
    match ast with
    | A_program tm -> type_of tm tenv



