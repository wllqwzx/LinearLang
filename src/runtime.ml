open Ast

type stack =
    | EmptyStack
    | ExtendStack   of string * value * stack

and heap = 
    | ExmptyHeap   
    | ExtendHeap    of int * value * heap

and value = 
    | NumVal        of int
    | BoolVal       of bool
    | AddrVal       of int
    | LambdaVal     of string * Ast.term * stack
    | LiStrVal      of string       (* in heap *)
    | LiConsVal     of int * int    (* in heap *)


let addressCount = ref 0;
let initStack = EmptyStack;
let initHeap = ref EmptyHeap;