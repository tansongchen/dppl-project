open Format
open Support.Error
open Support.Pervasive
open Mpi

let size = comm_size comm_world

let rank = comm_rank comm_world

(* ---------------------------------------------------------------------- *)
(* Datatypes *)

type unaryop =
    Positive of info
  | Negative of info
  | Succ of info
  | Pred of info

type binaryop =
    Plus of info
  | Minus of info
  | Times of info
  | Over of info
  | EQ of info
  | GT of info
  | LT of info

type ty =
    TyTop
  | TyVar of int * int
  | TyAll of string * ty * ty
  | TyId of string
  | TyArr of ty * ty
  | TyUnit
  | TyBool
  | TyInt
  | TyFloat
  | TyList of ty
  | TyAt of ty * int

type term =
    TmTAbs of info * string * ty * term
  | TmTApp of info * term * ty
  | TmFix of info * term
  | TmVar of info * int * int
  | TmAbs of info * string * ty * term
  | TmApp of info * term * term
  | TmBool of info * bool
  | TmIf of info * term * term * term
  | TmInt of info * int
  | TmUnit of info
  | TmFloat of info * float
  | TmUnary of info * unaryop * term
  | TmBinary of info * binaryop * term * term
  | TmNil of info * ty
  | TmCons of info * term * term
  | TmIsnil of info * term
  | TmHead of info * term
  | TmTail of info * term
  | TmAt of info * term * int

type binding =
    NameBind 
  | TyVarBind of ty
  | VarBind of ty
  | TyAbbBind of ty
  | TmAbbBind of term * (ty option)

type context = (string * binding) list

type command =
    Eval of info * term
  | Bind of info * string * binding

(* ---------------------------------------------------------------------- *)
(* Context management *)

let emptycontext = []

let ctxlength ctx = List.length ctx

let addbinding ctx x bind = (x,bind)::ctx

let addname ctx x = addbinding ctx x NameBind

let rec isnamebound ctx x =
  match ctx with
      [] -> false
    | (y,_)::rest ->
        if y=x then true
        else isnamebound rest x

let rec pickfreshname ctx x =
  if isnamebound ctx x then pickfreshname ctx (x^"'")
  else ((x,NameBind)::ctx), x

let index2name fi ctx x =
  try
    let (xn,_) = List.nth ctx x in
    xn
  with Failure _ ->
    let msg =
      Printf.sprintf "Variable lookup failure: offset: %d, ctx size: %d" in
    error fi (msg x (List.length ctx))

let rec name2index fi ctx x =
  match ctx with
      [] -> error fi ("Identifier " ^ x ^ " is unbound")
    | (y,_)::rest ->
        if y=x then 0
        else 1 + (name2index fi rest x)

(* ---------------------------------------------------------------------- *)
(* Shifting *)

let tymap onvar c tyT = 
  let rec walk c tyT = match tyT with
    TyTop -> TyTop
  | TyVar(x,n) -> onvar c x n
  | TyAll(tyX,tyT1,tyT2) -> TyAll(tyX,walk c tyT1,walk (c+1) tyT2)
  | TyArr(tyT1,tyT2) -> TyArr(walk c tyT1,walk c tyT2)
  | TyBool -> TyBool
  | TyInt -> TyInt
  | TyId(b) as tyT -> tyT
  | TyUnit -> TyUnit
  | TyFloat -> TyFloat
  | TyList(tyT1) -> TyList(walk c tyT1)
  | TyAt(tyT1,dis) -> TyAt(walk c tyT1,dis)
  in walk c tyT

let tmmap onvar ontype c t = 
  let rec walk c t = match t with
    TmTAbs(fi,tyX,tyT1,t2) ->
      TmTAbs(fi,tyX,ontype c tyT1,walk (c+1) t2)
  | TmTApp(fi,t1,tyT2) -> TmTApp(fi,walk c t1,ontype c tyT2)
  | TmVar(fi,x,n) -> onvar fi c x n
  | TmAbs(fi,x,tyT1,t2) -> TmAbs(fi,x,ontype c tyT1,walk (c+1) t2)
  | TmApp(fi,t1,t2) -> TmApp(fi,walk c t1,walk c t2)
  | TmBool(fi,t1) as t -> t
  | TmIf(fi,t1,t2,t3) -> TmIf(fi,walk c t1,walk c t2,walk c t3)
  | TmInt _ as t -> t
  | TmUnit(fi) as t -> t
  | TmFloat _ as t -> t
  | TmFix(fi,t1) -> TmFix(fi,walk c t1)
  | TmUnary(fi,op,t1) -> TmUnary(fi, op, walk c t1)
  | TmBinary(fi,op,t1,t2) -> TmBinary(fi, op, walk c t1, walk c t2)
  | TmNil(fi,tyT1) -> TmNil(fi, ontype c tyT1)
  | TmCons(fi,t1,t2) -> TmCons(fi, walk c t1, walk c t2)
  | TmIsnil(fi,t1) -> TmIsnil(fi, walk c t1)
  | TmHead(fi,t1) -> TmHead(fi, walk c t1)
  | TmTail(fi,t1) -> TmTail(fi, walk c t1)
  | TmAt(fi,t1,dis) -> TmAt(fi,walk c t1,dis)
  in walk c t

let typeShiftAbove d c tyT =
  tymap
    (fun c x n -> if x>=c then TyVar(x+d,n+d) else TyVar(x,n+d))
    c tyT

let termShiftAbove d c t =
  tmmap
    (fun fi c x n -> if x>=c then TmVar(fi,x+d,n+d) 
                     else TmVar(fi,x,n+d))
    (typeShiftAbove d)
    c t

let termShift d t = termShiftAbove d 0 t

let typeShift d tyT = typeShiftAbove d 0 tyT

let bindingshift d bind =
  match bind with
    NameBind -> NameBind
  | TyVarBind(tyS) -> TyVarBind(typeShift d tyS)
  | VarBind(tyT) -> VarBind(typeShift d tyT)
  | TyAbbBind(tyT) -> TyAbbBind(typeShift d tyT)
  | TmAbbBind(t,tyT_opt) ->
     let tyT_opt' = match tyT_opt with
                      None->None
                    | Some(tyT) -> Some(typeShift d tyT) in
     TmAbbBind(termShift d t, tyT_opt')

(* ---------------------------------------------------------------------- *)
(* Substitution *)

let termSubst j s t =
  tmmap
    (fun fi j x n -> if x=j then termShift j s else TmVar(fi,x,n))
    (fun j tyT -> tyT)
    j t

let termSubstTop s t = 
  termShift (-1) (termSubst 0 (termShift 1 s) t)

let typeSubst tyS j tyT =
  tymap
    (fun j x n -> if x=j then (typeShift j tyS) else (TyVar(x,n)))
    j tyT

let typeSubstTop tyS tyT = 
  typeShift (-1) (typeSubst (typeShift 1 tyS) 0 tyT)

let rec tytermSubst tyS j t =
  tmmap (fun fi c x n -> TmVar(fi,x,n))
        (fun j tyT -> typeSubst tyS j tyT) j t

let tytermSubstTop tyS t = 
  termShift (-1) (tytermSubst (typeShift 1 tyS) 0 t)

(* ---------------------------------------------------------------------- *)
(* Context management (continued) *)

let rec getbinding fi ctx i =
  try
    let (_,bind) = List.nth ctx i in
    bindingshift (i+1) bind 
  with Failure _ ->
    let msg =
      Printf.sprintf "Variable lookup failure: offset: %d, ctx size: %d" in
    error fi (msg i (List.length ctx))
 let getTypeFromContext fi ctx i =
   match getbinding fi ctx i with
         VarBind(tyT) -> tyT
     | TmAbbBind(_,Some(tyT)) -> tyT
     | TmAbbBind(_,None) -> error fi ("No type recorded for variable "
                                        ^ (index2name fi ctx i))
     | _ -> error fi 
       ("getTypeFromContext: Wrong kind of binding for variable " 
         ^ (index2name fi ctx i)) 
(* ---------------------------------------------------------------------- *)
(* Extracting file info *)

let tmInfo t = match t with
    TmTAbs(fi,_,_,_) -> fi
  | TmTApp(fi,_, _) -> fi
  | TmVar(fi,_,_) -> fi
  | TmAbs(fi,_,_,_) -> fi
  | TmApp(fi, _, _) -> fi
  | TmBool(fi, _) -> fi
  | TmIf(fi,_,_,_) -> fi
  | TmInt(fi,_) -> fi
  | TmUnit(fi) -> fi
  | TmFloat(fi,_) -> fi
  | TmFix(fi,_) -> fi 
  | TmUnary(fi,_,_) -> fi
  | TmBinary(fi,_,_,_) -> fi
  | TmNil(fi,_) -> fi
  | TmCons(fi,_,_) -> fi
  | TmIsnil(fi,_) -> fi
  | TmHead(fi,_) -> fi
  | TmTail(fi,_) -> fi
  | TmAt(fi,_,_) -> fi

(* ---------------------------------------------------------------------- *)
(* Printing *)

(* The printing functions call these utility functions to insert grouping
  information and line-breaking hints for the pretty-printing library:
     obox   Open a "box" whose contents will be indented by two spaces if
            the whole box cannot fit on the current line
     obox0  Same but indent continuation lines to the same column as the
            beginning of the box rather than 2 more columns to the right
     cbox   Close the current box
     break  Insert a breakpoint indicating where the line maybe broken if
            necessary.
  See the documentation for the Format module in the OCaml library for
  more details. 
*)

let obox0() = open_hvbox 0
let obox() = open_hvbox 2
let cbox() = close_box()
let break() = print_break 0 0

let small t = 
  match t with
    TmVar(_,_,_) -> true
  | _ -> false

let rec printty_Type outer ctx tyT = match tyT with
    TyAll(tyX,tyT1,tyT2) ->
      let (ctx1,tyX) = (pickfreshname ctx tyX) in
      obox(); pr "All "; pr tyX;
      proty ctx tyT1;
      pr ".";
      print_space ();
      printty_Type outer ctx1 tyT2;
      cbox()
  | TyList(tyT1) -> pr "List "; printty_AType false ctx tyT1
  | TyAt(tyT1,dis) -> printty_AType false ctx tyT1; pr " @ "; pr (string_of_int dis)
  | tyT -> printty_ArrowType outer ctx tyT

and proty ctx tyS =
  if tyS <> TyTop then (pr "<:"; printty_Type false ctx tyS)

and printty_ArrowType outer ctx  tyT = match tyT with 
    TyArr(tyT1,tyT2) ->
      obox0(); 
      printty_AType false ctx tyT1;
      if outer then pr " ";
      pr "->";
      if outer then print_space() else break();
      printty_ArrowType outer ctx tyT2;
      cbox()
  | tyT -> printty_AType outer ctx tyT

and printty_AType outer ctx tyT = match tyT with
    TyTop -> pr "Top"
  | TyVar(x,n) ->
      if ctxlength ctx = n then
        pr (index2name dummyinfo ctx x)
      else
        pr ("[bad index: " ^ (string_of_int x) ^ "/" ^ (string_of_int n)
            ^ " in {"
            ^ (List.fold_left (fun s (x,_) -> s ^ " " ^ x) "" ctx)
            ^ " }]")
  | TyBool -> pr "Bool"
  | TyInt -> pr "Int"
  | TyUnit -> pr "Unit"
  | TyId(b) -> pr b
  | TyFloat -> pr "Float"
  | tyT -> pr "("; printty_Type outer ctx tyT; pr ")"

let printty ctx tyT = printty_Type true ctx tyT 

let rec printtm_Term outer ctx t = match t with
    TmTAbs(fi,x,tyS,t) ->
      (let (ctx1,x) = (pickfreshname ctx x) in
               obox(); pr "lambda "; pr x;
               proty ctx tyS;
               pr ".";
               if (small t) && not outer then break() else print_space();
               printtm_Term outer ctx1 t;
               cbox())
  | TmAbs(fi,x,tyT1,t2) ->
      (let (ctx',x') = (pickfreshname ctx x) in
         obox(); pr "lambda ";
         pr x'; pr ":"; printty_Type false ctx tyT1; pr ".";
         if (small t2) && not outer then break() else print_space();
         printtm_Term outer ctx' t2;
         cbox())
  | TmIf(fi, t1, t2, t3) ->
       obox0();
       pr "if ";
       printtm_Term false ctx t1;
       print_space();
       pr "then ";
       printtm_Term false ctx t2;
       print_space();
       pr "else ";
       printtm_Term false ctx t3;
       cbox()
  | TmFix(fi, t1) ->
       obox();
       pr "fix "; 
       printtm_Term false ctx t1;
       cbox()
  | t -> printtm_AppTerm outer ctx t

and printtm_AppTerm outer ctx t = match t with
    TmTApp(fi,t,tyS) ->
      obox0();
      printtm_AppTerm false ctx t;
      print_space();
      pr "["; printty_Type false ctx tyS; pr "]";
      cbox()
  | TmApp(fi, t1, t2) ->
      obox0();
      printtm_AppTerm false ctx t1;
      print_space();
      printtm_ATerm false ctx t2;
      cbox()
  | TmBinary(_,op,t1,t2) ->
       pr "binary "; printtm_ATerm false ctx t1; 
       pr " "; printtm_ATerm false ctx t2 
  | TmIsnil(_, t1) ->
       obox();
       pr "isnil ";
       printtm_ATerm false ctx t1;
       cbox()
  | TmHead(_, t1) ->
       obox();
       pr "head ";
       printtm_ATerm false ctx t1;
       cbox()
  | TmTail(_, t1) ->
       obox();
       pr "tail ";
       printtm_ATerm false ctx t1;
       cbox()
  | t -> printtm_ATerm outer ctx t

and printtm_ATerm outer ctx t = match t with
    TmVar(fi,x,n) ->
      if ctxlength ctx = n then
        pr (index2name fi ctx x)
      else
        pr ("[bad index: " ^ (string_of_int x) ^ "/" ^ (string_of_int n)
            ^ " in {"
            ^ (List.fold_left (fun s (x,_) -> s ^ " " ^ x) "" ctx)
            ^ " }]")
  | TmBool(_,b) -> print_bool b
  | TmInt(fi,i) ->
       pr (string_of_int i)
  | TmUnit(_) -> pr "unit"
  | TmFloat(_,s) -> pr (string_of_float s)
  | TmNil(_,tyT1) -> pr "[]"
  (* | TmNil(_,tyT1) -> pr "[] as List "; printty_Type false ctx tyT1 *)
  | TmCons(_,t1,t2) as c -> 
      let rec f t= match t with
          TmNil(_,tyT1) -> pr "[]"
        | TmCons(_,v1,v2) -> 
          printtm_Term false ctx v1; pr " :: "; f v2;
        | _ -> printtm_Term false ctx t1; pr " :: "; printtm_Term false ctx t2
      in f c
  | TmAt(_, t1, p) ->
      obox();
      printtm_ATerm false ctx t1;
      pr " @ ";
      pr (string_of_int p);
      cbox()
  | t -> pr "("; printtm_Term outer ctx t; pr ")"

let printtm ctx t = printtm_Term true ctx t 

let prbinding ctx b = match b with
    NameBind -> ()
  | TyVarBind(tyS) -> pr "<: ";printty_Type false ctx tyS
  | VarBind(tyT) -> pr ": "; printty ctx tyT
  | TyAbbBind(tyT) -> pr "= "; printty ctx tyT
  | TmAbbBind(t,tyT) -> pr "= "; printtm ctx t 
