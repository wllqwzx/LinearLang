type ty =
    | NumTy
    | BoolTy
    | LinResTy
    | LinListTy
    | UnitTy
    | ArrowTy        of ty * ty

type term =
    | Num_term       of int
    | Add_term       of term * term 
    | Sub_term       of term * term
    | Mul_term       of term * term
    | Le_term        of term * term
    | Eq_term        of term * term
    | Bool_term      of bool
    | And_term       of term * term
    | Or_term        of term * term
    | Not_term       of term

    | Var_term       of string
    | If_term        of term * term * term
    | If_null_term   of string * term * term
    | Lambda_term    of string * ty * term
    | App_term       of string * term
    | Begin_term     of term list
    | LetUn_term     of string * term * term
    | LetLin_term    of string * term * term
    | Letrec_term    of string * ty * term * term
    
    | NewLinRes_term    of string
    | CopyAtom_term     of term * string * string * term
    | CopyList_term     of term * string * string * term
    | Split_term        of term * string * string * term
    | FreeAtom_term     of term
    | FreeList_term     of term
    | Print_term        of term
    | LinCons_term      of term * term
    | Null_term


type program =
    | A_program of term
