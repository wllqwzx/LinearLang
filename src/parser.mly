%token L_PAREN
%token R_PAREN
%token L_CURLY
%token R_CURLY
%token EQUAL
%token COMMA
%token SEMICOLEN
%token COLEN
%token DOUBLEQUOTE

%token LETUN
%token LETLIN
%token LETREC
%token IN
%token IF
%token IFNULL
%token THEN
%token ELSE
%token FUN
%token BEGIN
%token END

%token NEWRES
%token LINCONS
%token NULL
%token SPLIT
%token AS

%token COPYATOM
%token COPYLIST
%token FREEATOM
%token FREELIST
%token PRINT
%token AND
%token OR
%token NOT
%token ADD
%token SUB
%token MUL
%token LE
%token EQ


%token NUMBER
%token BOOL
%token LINLIST
%token LINRES
%token UNIT
%token ARROW

%token <int>NUM
%token <string>ID
%token <string>STR
%token <bool>BOOLVAL

%token EOF

%start <Ast.program> enterPoint
%%

enterPoint:
    | ast = nt_program; EOF         { ast }

nt_program:
    | ast = nt_term                  { Ast.A_program ast }

nt_term:
    | ast = NUM                     { Ast.Num_term ast }
    | ADD; L_PAREN; ast1=nt_term; COMMA; ast2=nt_term; R_PAREN
                                    { Ast.Add_term (ast1,ast2) }
    | SUB; L_PAREN; ast1=nt_term; COMMA; ast2=nt_term; R_PAREN
                                    { Ast.Sub_term (ast1,ast2) }
    | MUL; L_PAREN; ast1=nt_term; COMMA; ast2=nt_term; R_PAREN
                                    { Ast.Mul_term (ast1,ast2) }                                    
    | LE; L_PAREN; ast1=nt_term; COMMA; ast2=nt_term; R_PAREN
                                    { Ast.Le_term (ast1,ast2) }
    | EQ; L_PAREN; ast1=nt_term; COMMA; ast2=nt_term; R_PAREN
                                    { Ast.Eq_term (ast1,ast2) }
    | ast = BOOLVAL                 { Ast.Bool_term ast }
    | AND; L_PAREN; ast1=nt_term; COMMA; ast2=nt_term; R_PAREN
                                    { Ast.And_term (ast1,ast2) }
    | OR; L_PAREN; ast1=nt_term; COMMA; ast2=nt_term; R_PAREN
                                    { Ast.Or_term (ast1,ast2) }
    | NOT; L_PAREN; ast1=nt_term; R_PAREN
                                    { Ast.Not_term ast1 }

    | ast = ID                      { Ast.Var_term ast }
    | LETUN; ast1 = ID; EQUAL; ast2 = nt_term; ast3 = nt_term
                                    { Ast.LetUn_term (ast1,ast2,ast3) }
    | LETLIN; ast1 = ID; EQUAL; ast2 = nt_term; ast3 = nt_term
                                    { Ast.LetLin_term (ast1,ast2,ast3) }
    | LETREC; ast1 = ID; COLEN; typ = nt_ty ; EQUAL; ast2 = nt_term; ast3 = nt_term
                                    { Ast.Letrec_term (ast1,typ,ast2,ast3) }
    | IF; ast1 = nt_term; THEN; ast2 = nt_term; ELSE; ast3 = nt_term
                                    { Ast.If_term (ast1,ast2,ast3) }
    | IFNULL; ast1 = ID; THEN; ast2 = nt_term; ELSE; ast3 = nt_term
                                    { Ast.If_null_term (ast1,ast2,ast3) }
    | FUN; L_PAREN; ast1 = ID; COLEN; typ = nt_ty; R_PAREN; L_CURLY; ast2 = nt_term; R_CURLY
                                    { Ast.Lambda_term (ast1,typ,ast2) }
    | ast = ID; L_PAREN; ast1 = nt_term; R_PAREN
                                    { Ast.App_term (ast,ast1) }
    | BEGIN; astlist = separated_list (SEMICOLEN, nt_term); END
                                    { Ast.Begin_term astlist }

    | NEWRES; L_PAREN; ast=STR; R_PAREN
                                    { Ast.NewLinRes_term ast } 
    | COPYATOM; ast = nt_term; AS; ast1 = ID; ast2 = ID; ast3 = nt_term
                                    { Ast.CopyAtom_term (ast,ast1,ast2,ast3) }
    | COPYLIST; ast = nt_term; AS; ast1 = ID; ast2 = ID; ast3 = nt_term
                                    { Ast.CopyList_term (ast,ast1,ast2,ast3) }
    | FREEATOM; L_PAREN; ast=nt_term; R_PAREN
                                    { Ast.FreeAtom_term ast }
    | FREELIST; L_PAREN; ast=nt_term; R_PAREN
                                    { Ast.FreeList_term ast }
    | PRINT; L_PAREN; ast = nt_term; R_PAREN
                                    { Ast.Print_term ast }
    | LINCONS; L_PAREN; ast1 = nt_term; COMMA; ast2 = nt_term; R_PAREN
                                    { Ast.LinCons_term (ast1,ast2) }
    | SPLIT; ast = nt_term; AS; ast1 = ID; ast2 = ID; ast3 = nt_term
                                    { Ast.Split_term (ast,ast1,ast2,ast3) }
    | NULL                          { Ast.Null_term }


nt_ty:
    | NUMBER    { Ast.NumTy }
    | BOOL      { Ast.BoolTy }
    | LINRES    { Ast.LinResTy }
    | LINLIST   { Ast.LinListTy }
    | UNIT      { Ast.UnitTy }
    | ARROW; L_PAREN; ty1 = nt_ty; COMMA; ty2 = nt_ty; R_PAREN
                { Ast.ArrowTy (ty1,ty2) }

