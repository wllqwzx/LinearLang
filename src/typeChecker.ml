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


let envCopy = 
    fun envref ->
    ref (!envref)


let rec envSame =
    fun env1 env2 ->
    match env1, env2 with
    | Empty_tenv, Empty_tenv -> true
    | Extend_tenv (str1,typ1,ev1), Extend_tenv (str2,typ2,ev2) ->
                if (String.equal str1 str2)
                then (typ1 = typ2) && (envSame ev1 ev2)
                else false
    | _ -> false



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
    | If_term        (tm1,tm2,tm3) -> let typ = type_of tm1 in
                                      if typ = BoolTy
                                      then let tenvCopy = envCopy tenv in
                                           let typ1 = type_of tm2 in
                                           let temp = envCopy tenv in
                                           begin
                                                tenv := !tenvCopy;
                                                let typ2 = type_of tm3 in
                                                if typ1 = typ2 
                                                then if envSame !temp !tenv
                                                     then typ1
                                                     else raise (TypeError ("type context is different after two branches!"))
                                                else raise (TypeError ("type is not equal in if branches!"))
                                           end
                                      else raise (TypeError ("term in if condition should be BoolTy!"))
    | If_null_term   (str1,tm1,tm2) -> let typ = lookup_type !tenv str1 in
                                       if typ = LinListTy
                                       then let tenvCopy = envCopy tenv in
                                           let typ1 = type_of tm1 in
                                           let temp = envCopy tenv in
                                           begin
                                                tenv := !tenvCopy;
                                                let typ2 = type_of tm2 in
                                                if typ1 = typ2 
                                                then if envSame !temp !tenv
                                                     then typ1
                                                     else raise (TypeError ("type context is different after two branches!"))
                                                else raise (TypeError ("type is not equal in if branches!"))
                                           end
                                       else raise (TypeError ("term in if-null condition should be LinListTy!"))
    | Lambda_term    (str,typ,tm) -> let envSave = (envCopy tenv) in
                                     let env = !tenv in
                                     begin
                                        tenv := (Extend_tenv (str,typ,env));
                                        let typ2 = type_of tm in
                                        begin
                                            tenv := delete_var !tenv str;
                                            if (envSame !tenv !envSave)
                                            then ArrowTy (typ,typ2)
                                            else raise (TypeError ("Can not use LinRes defined outside the function!")) 
                                        end
                                     end
    | App_term       (str,tm) -> let typ = lookup_type !tenv str in
                                 (match typ with
                                  | ArrowTy (ty1,ty2) -> let typ2 = type_of tm in
                                                         if ty1 = typ2 
                                                         then ty2
                                                         else raise (TypeError ("types of the parameter does not match the function: " ^ str))
                                  | _ -> raise (TypeError (str ^ " should be a function!")))
    | Begin_term     tmli -> type_of_tmli tmli
    | LetUn_term     (str,tm1,tm2) -> let typ = type_of tm1 in
                                     (match typ with
                                      | LinResTy -> raise (TypeError ("LetUn can not bind a LinResTy term!"))
                                      | LinListTy -> raise (TypeError ("LetUn can not bind a LinListTy term!"))
                                      | _ -> begin 
                                                tenv := Extend_tenv (str,typ,!tenv);
                                                let typ0 = type_of tm2 in
                                                begin 
                                                    tenv := delete_var !tenv str;
                                                    typ0
                                                end
                                             end)
    | LetLin_term    (str,tm1,tm2) -> let typ = type_of tm1 in
                                      (match typ with
                                      | LinResTy -> begin
                                                        tenv := Extend_tenv (str,typ,!tenv);
                                                        let typ0 = type_of tm2 in
                                                        begin
                                                            tenv := delete_var !tenv str;
                                                            typ0
                                                        end
                                                    end
                                      | LinListTy -> begin
                                                        tenv := Extend_tenv (str,typ,!tenv);
                                                        let typ0 = type_of tm2 in
                                                        begin
                                                            tenv := delete_var !tenv str;
                                                            typ0
                                                        end
                                                     end
                                      | _ -> raise (TypeError ("LetLin can not bind a unrestricted term!")))
    (*| Letrec_term    (str,typ,tm1,tm2) -> *)
    | Fix_term          tm -> let typ = type_of tm in
                              (match typ with
                              | ArrowTy (ty1,ty2) -> if ty1 = ty2
                                                     then ty1
                                                     else raise (TypeError ("term in fix should have type: T -> T"))
                              | _ -> raise (TypeError ("fix should receive a function!")))
    
(*--------------------*)
    | NewLinRes_term    str -> LinResTy
    | CopyAtom_term     (tm1,str1,str2,tm2) -> let typ = type_of tm1 in
                                               (match typ with
                                               | LinResTy -> begin
                                                                tenv := (Extend_tenv (str1,typ,(Extend_tenv (str2,typ,!tenv))));
                                                                let typ0 = type_of tm2 in
                                                                begin
                                                                    tenv := delete_var !tenv str1;
                                                                    tenv := delete_var !tenv str2;
                                                                    typ0
                                                                end
                                                             end
                                               | _ -> raise (TypeError ("copyAtom only accept LinResTy!")))
    | CopyList_term     (tm1,str1,str2,tm2) -> let typ = type_of tm1 in
                                               (match typ with
                                               | LinListTy -> begin
                                                                tenv := (Extend_tenv (str1,typ,(Extend_tenv (str2,typ,!tenv))));
                                                                let typ0 = type_of tm2 in
                                                                begin
                                                                    tenv := delete_var !tenv str1;
                                                                    tenv := delete_var !tenv str2;
                                                                    typ0
                                                                end
                                                              end
                                               | _ -> raise (TypeError ("copyList only accept LinListTy!")))
    | Split_term        (tm1,str1,str2,tm2) -> let typ = type_of tm1 in
                                               (match typ with
                                               | LinListTy -> begin
                                                                tenv := (Extend_tenv (str1,LinResTy,(Extend_tenv (str2,LinListTy,!tenv))));
                                                                let typ0 = type_of tm2 in
                                                                begin
                                                                    tenv := delete_var !tenv str1;
                                                                    tenv := delete_var !tenv str2;
                                                                    typ0
                                                                end
                                                              end
                                               | _ -> raise (TypeError ("split only accept LinListTy!")))
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

and type_of_tmli = 
    fun tmli ->
    match tmli with
    | [] -> raise (TypeError ("begin term should contain at least one term!"))
    | tm::[] -> type_of tm
    | tm::tmli0 -> let typ = type_of tm in
                   if typ = UnitTy
                   then type_of_tmli tmli0
                   else raise (TypeError ("terms in begin should have UnitTy except the last one!"))


let check =
    fun ast ->
    match ast with
    | A_program tm -> type_of tm


