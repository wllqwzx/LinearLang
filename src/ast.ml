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
    | Le_term        of term * term
    | Eq_term        of term * term
    | Bool_term      of bool
    | And_term       of term * term
    | Or_term        of term * term
    | Not_term       of term * term

    | Var_term       of string
    | LetUn_term     of string * term * term
    | LetLin_term    of string * term * term
    | If_term        of term * term * term
    | Lambda_term    of string * ty * term
    | App_trem       of term * term
    | Begin_term     of term list

    | NewLinRes_term        of string
    | CopyLinRes_term       of string * string * term
    | FreeLinRes_term       of term
    | PrintLinRes_term      of term
    | LinCons_term      of term * term
    | Split_term        of string * string * term
    | LinListNull_term


type program =
    | A_program of term
