open Ast

exception RuntimeError of string

type stackData =
    | EmptyStack
    | ExtendStack   of string * value * stackData

and heapData = 
    | EmptyHeap   
    | ExtendHeap    of int * value * heapData

and value = 
    | NumVal        of int          (* in stack *)
    | BoolVal       of bool         (* in stack *)
    | AddrVal       of int          (* in stack: reference of LinRes / LinList *)
    | UnitVal                       (* in stack *)
    | ClosureVal    of string * Ast.term * stackData    (* in stack *)
    | FixVal        of string * Ast.term * stackData    (* in stack *)
    | LinResVal     of string       (* in heap *)
    | LinListVal    of int * int    (* in heap *)


let print_val =
    fun v ->
    match v with
    | NumVal        n -> print_string (string_of_int n)
    | BoolVal       b -> print_string (string_of_bool b)
    | AddrVal       addr -> print_string "reference"
    | UnitVal       -> print_string "Unit"
    | ClosureVal    (str,tm,stk) -> print_string "closure"
    | FixVal        (str,tm,stk) -> print_string "fix point"
    | _             -> print_string "print_val error!" 


let addressCount = ref 0
let stack = EmptyStack
let heap = ref EmptyHeap

let rec lookup_stack =
    fun stk str -> 
    match stk with
    | EmptyStack -> raise (RuntimeError ("undefined variable: " ^ str))
    | ExtendStack (s,v,stk0) -> if (String.equal s str)
                                    then v
                                    else lookup_stack stk0 str


let rec lookup_heap =
    fun hp addr ->
    match hp with
    | EmptyHeap -> raise (RuntimeError ("defeference an unalloced address in heap!"))
    | ExtendHeap (a,v,h0) -> if a = addr
                             then v
                             else lookup_heap h0 addr

let rec delete_heap =
    fun hp addr ->
    match hp with
    | EmptyHeap -> EmptyHeap
    | ExtendHeap (a,v,h) -> if a = addr
                            then h
                            else ExtendHeap (a,v,(delete_heap h addr))

let rec removeLinList =
    fun hp addr ->
    let hv = lookup_heap hp addr in
    match hv with
    | LinListVal (addr1,addr2) -> if addr1 = -1 && addr2 = -1
                                  then delete_heap hp addr
                                  else let hp2 = delete_heap hp addr1 in
                                       removeLinList hp2 addr2
    | _ -> raise (RuntimeError ("removeList only accept LinList!"))


let rec print_addr =
    fun hp addr ->
    match hp with
    | EmptyHeap -> raise (RuntimeError ("print_addr can not find the addr in heap!"))
    | ExtendHeap (a,v,h) -> if a = addr
                            then (match v with
                                  | LinResVal str -> print_string ("(" ^ str ^ ") ")
                                  | LinListVal (addr1,addr2) -> if addr1 = -1 && addr2 = -1
                                                                then print_string "null "
                                                                else begin
                                                                        print_addr !heap addr1;
                                                                        print_addr !heap addr2
                                                                     end
                                  | _ -> raise (RuntimeError ("error! heap can not store non resource value!")))
                            else print_addr h addr

let rec changeAddr =
    fun hp addrx addr2 ->
    match hp with
    | EmptyHeap -> raise (RuntimeError ("changeAddr error!"))
    | ExtendHeap (a,v,h) -> if a = addr2
                            then ExtendHeap (addrx,v,h) 
                            else ExtendHeap (a,v,(changeAddr h addrx addr2))

let rec appendAddr =
    fun addrx addr2 -> 
    let hv = lookup_heap !heap addrx in
    match hv with
    | LinListVal (x1,x2) -> if x1 = -1 && x2 = -1
                            then begin
                                     heap := delete_heap !heap addrx;
                                     heap := changeAddr !heap addrx addr2
                                 end
                            else appendAddr x2 addr2
    | _ -> raise (RuntimeError ("appendAddr error1"))


let rec copyListTo =
    fun addr newAddr ->
    let hv = lookup_heap !heap addr in
    match hv with
    | LinListVal (left,right) -> if left = -1 && right = -1
                                 then heap := ExtendHeap (newAddr,(LinListVal (-1,-1)),!heap)
                                 else let lv = lookup_heap !heap left in
                                      begin
                                        addressCount := !addressCount + 2;
                                        let newLeftAddr = !addressCount -1 in
                                        let newRightAddr = !addressCount in
                                        begin
                                            heap := ExtendHeap (newAddr,(LinListVal (newLeftAddr,newRightAddr)),!heap);
                                            heap := ExtendHeap (newLeftAddr,lv,!heap);
                                            copyListTo right newRightAddr
                                        end
                                      end
    | _ -> raise (RuntimeError ("copyListTo error!"))


let rec value_of_tm =
    fun tm stk ->
    match tm with
    | Num_term       n -> NumVal n
    | Add_term       (tm1, tm2) -> let val1 = value_of_tm tm1 stk in
                                   let val2 = value_of_tm tm2 stk in
                                   (match val1,val2 with
                                   | NumVal n1, NumVal n2 -> NumVal (n1+n2)
                                   | _ -> raise (RuntimeError ("error in add!")))
    | Sub_term       (tm1, tm2) -> let val1 = value_of_tm tm1 stk in
                                   let val2 = value_of_tm tm2 stk in
                                   (match val1,val2 with
                                   | NumVal n1, NumVal n2 -> NumVal (n1-n2)
                                   | _ -> raise (RuntimeError ("error in sub!")))
    | Mul_term       (tm1, tm2) -> let val1 = value_of_tm tm1 stk in
                                   let val2 = value_of_tm tm2 stk in
                                   (match val1,val2 with
                                   | NumVal n1, NumVal n2 -> NumVal (n1*n2)
                                   | _ -> raise (RuntimeError ("error in mul!")))
    | Le_term        (tm1, tm2) -> let val1 = value_of_tm tm1 stk in
                                   let val2 = value_of_tm tm2 stk in
                                   (match val1,val2 with
                                   | NumVal n1, NumVal n2 -> if n1 < n2 then BoolVal true else BoolVal false
                                   | _ -> raise (RuntimeError ("error in le!")))
    | Eq_term        (tm1, tm2) -> let val1 = value_of_tm tm1 stk in
                                   let val2 = value_of_tm tm2 stk in
                                   (match val1,val2 with
                                   | NumVal n1, NumVal n2 -> if n1 = n2 then BoolVal true else BoolVal false
                                   | _ -> raise (RuntimeError ("error in eq!")))
    | Bool_term      b -> BoolVal b
    | And_term       (tm1, tm2) -> let val1 = value_of_tm tm1 stk in
                                   let val2 = value_of_tm tm2 stk in
                                   (match val1,val2 with
                                   | BoolVal b1, BoolVal b2 -> if b1 && b2 then BoolVal true else BoolVal false
                                   | _ -> raise (RuntimeError ("error in and!")))
    | Or_term        (tm1, tm2) -> let val1 = value_of_tm tm1 stk in
                                   let val2 = value_of_tm tm2 stk in
                                   (match val1,val2 with
                                   | BoolVal b1, BoolVal b2 -> if b1 || b2 then BoolVal true else BoolVal false
                                   | _ -> raise (RuntimeError ("error in or!")))

    | Not_term       tm1 -> let val1 = value_of_tm tm1 stk in
                           (match val1 with
                           | BoolVal b -> if b then BoolVal false else BoolVal true
                           | _ -> raise (RuntimeError ("error in not!")))

    | Var_term       str -> lookup_stack stk str
    | If_term        (tm1,tm2,tm3) -> let bv = value_of_tm tm1 stk in
                                      (match bv with
                                      | BoolVal b -> if b 
                                                     then value_of_tm tm2 stk
                                                     else value_of_tm tm3 stk
                                      | _ -> raise (RuntimeError ("Runtime error in if condition!")))
    | If_null_term   (str,tm1,tm2) -> let v = lookup_stack stk str in
                                      (match v with
                                      | AddrVal addr -> let hv = lookup_heap !heap addr in
                                                        (match hv with
                                                        | LinListVal (addr1,addr2) -> if addr1 = -1 && addr2 = -1
                                                                                      then value_of_tm tm1 stk
                                                                                      else value_of_tm tm2 stk
                                                        | _ -> raise (RuntimeError ("runtime error in if-null condition")))
                                      | _ -> raise (RuntimeError ("runtime error in if-null condition")))
    | Lambda_term    (str,typ,tm) -> ClosureVal (str,tm,stk)
    | App_term       (tm1,tm2) -> let clo = value_of_tm tm1 stk in
                                  (match clo with
                                  | ClosureVal (str,tm,stk0) -> let v = value_of_tm tm2 stk in
                                                                value_of_tm tm (ExtendStack (str,v,stk0))
                                  | FixVal (str,tm,stk0) -> let op = value_of_tm tm (ExtendStack (str,clo,stk0)) in
                                                            let v = value_of_tm tm2 stk in
                                                            (match op with
                                                            | ClosureVal (str2,tm2,stk2) -> value_of_tm tm2 (ExtendStack (str2,v,stk2))
                                                            | _ -> raise (RuntimeError ("FixVal error!")))
                                  | _ -> raise (RuntimeError ("need a function!")))
    | Begin_term     tmlist -> value_of_tmlist tmlist stk
    | LetUn_term     (str,tm1,tm2) -> let v = value_of_tm tm1 stk in
                                      value_of_tm tm2 (ExtendStack (str,v,stk))
    | LetLin_term    (str,tm1,tm2) -> let v = value_of_tm tm1 stk in
                                      value_of_tm tm2 (ExtendStack (str,v,stk))
    (*| Letrec_term    of string * ty * term * term*)
    | Fix_term          tm -> let v = value_of_tm tm stk in
                              (match v with
                              | ClosureVal (str,tm0,stk0) -> value_of_tm tm0 (ExtendStack (str,(FixVal (str,tm0,stk0)),stk0))
                              | _ -> raise (RuntimeError ("fix operator accept lambda term!")))
    | NewLinRes_term    str -> addressCount := !addressCount + 1;
                               heap := ExtendHeap (!addressCount,(LinResVal str),!heap);
                               AddrVal !addressCount
    | CopyAtom_term     (tm1,str1,str2,tm2) ->  let v = value_of_tm tm1 stk in
                                                (match v with
                                                | AddrVal addr -> 
                                                        let hv = lookup_heap !heap addr in
                                                        (match hv with
                                                        | LinResVal str -> 
                                                                begin
                                                                    addressCount := !addressCount + 1;
                                                                    heap := ExtendHeap (!addressCount,LinResVal str,!heap);
                                                                    value_of_tm tm2 (ExtendStack (str1,AddrVal addr,(ExtendStack (str2,AddrVal ! addressCount,stk))))
                                                                end
                                                        | _ -> raise (RuntimeError ("copyAtom only accept LinRes!")))
                                                | _ -> raise (RuntimeError ("copyAtom only accept resources!")))
    | CopyList_term     (tm1,str1,str2,tm2) -> let v = value_of_tm tm1 stk in
                                               (match v with
                                               | AddrVal addr -> begin
                                                                    addressCount := !addressCount + 1;
                                                                    let newAddr = !addressCount in
                                                                    begin
                                                                        copyListTo addr newAddr;
                                                                        value_of_tm tm2 (ExtendStack (str1,AddrVal addr,(ExtendStack (str2,AddrVal newAddr,stk))))
                                                                    end
                                                                 end
                                               | _ -> raise (RuntimeError ("copylist accept resources!")))
    | AppendList_term   (tm1,tm2) -> let v1 = value_of_tm tm1 stk in
                                     let v2 = value_of_tm tm2 stk in
                                     (match v1,v2 with
                                     | AddrVal addr1, AddrVal addr2 -> 
                                            let hv1 = lookup_heap !heap addr1 in
                                            let hv2 = lookup_heap !heap addr2 in
                                            (match hv1,hv2 with
                                            | LinListVal (a1,a2), LinListVal (b1,b2) -> 
                                                if a1 = -1 && a2 = -1
                                                then begin 
                                                        heap := delete_heap !heap addr1;
                                                        AddrVal addr2
                                                     end
                                                else begin
                                                        appendAddr a2 addr2;
                                                        AddrVal addr1
                                                     end
                                            | _ -> raise (RuntimeError ("appendList only accepty LinList!")))
                                     | _ -> raise (RuntimeError ("appendList only accept resources!")))
    | Split_term        (tm1,str1,str2,tm2) -> let v = value_of_tm tm1 stk in
                                               (match v with
                                               | AddrVal addr -> 
                                                    let hv = lookup_heap !heap addr in
                                                    (match hv with
                                                    | LinListVal (addr1,addr2) -> 
                                                        if addr1 = -1 && addr2 = -1
                                                        then begin 
                                                                addressCount := !addressCount + 1;
                                                                heap := ExtendHeap (!addressCount,(LinListVal (-1,-1)),!heap);
                                                                value_of_tm tm2 (ExtendStack (str1,AddrVal addr,(ExtendStack (str2,AddrVal !addressCount,stk))))
                                                             end
                                                        else begin
                                                                heap := delete_heap !heap addr;
                                                                value_of_tm tm2 (ExtendStack (str1,AddrVal addr1,(ExtendStack (str2,AddrVal addr2,stk))))
                                                             end
                                                    | _ -> raise (RuntimeError ("split LinList only!")))
                                               | _ -> raise (RuntimeError ("split resourcees only!")))
    | FreeAtom_term     tm -> let v = value_of_tm tm stk in
                              (match v with
                              | AddrVal addr -> let hv = lookup_heap !heap addr in
                                                (match hv with
                                                | LinResVal str -> begin
                                                                        heap := delete_heap !heap addr;
                                                                        UnitVal
                                                                   end
                                                | _ -> raise (RuntimeError ("freeAtom only accept LinRes!")))
                              | _ -> raise (RuntimeError ("freeAtom only accept resources!")))
    | FreeList_term     tm -> let v = value_of_tm tm stk in
                              (match v with
                              | AddrVal addr -> begin
                                                    heap := removeLinList !heap addr;
                                                    UnitVal
                                                end
                              | _ -> raise (RuntimeError ("freeList only accept resources!")))
    | Print_term        tm -> let v = value_of_tm tm stk in
                              (match v with
                              | AddrVal addr -> print_addr !heap addr; v
                              | _ -> raise (RuntimeError ("print only accept resources!")))
    | LinCons_term      (tm1,tm2) -> let v1 = value_of_tm tm1 stk in
                                     let v2 = value_of_tm tm2 stk in
                                     (match v1,v2 with
                                     | AddrVal n1, AddrVal n2 -> addressCount := !addressCount + 1;
                                                                 heap := ExtendHeap (!addressCount,(LinListVal (n1,n2)),!heap);
                                                                 AddrVal !addressCount
                                     | _ -> raise (RuntimeError ("LinCons need a LinRes and a LinList!")))
    | Null_term         ->  addressCount := !addressCount + 1;
                            heap := ExtendHeap (!addressCount,(LinListVal (-1,-1)),!heap);
                            AddrVal !addressCount

and value_of_tmlist =
    fun tmlist stk ->
    match tmlist with
    | [] -> raise (RuntimeError ("begin can not empty term"))
    | tm::[] -> value_of_tm tm stk
    | tm::li -> let _ = value_of_tm tm stk in
                value_of_tmlist li stk



let value_of_program =
    fun ast ->
    match ast with
    | A_program tm -> value_of_tm tm stack


