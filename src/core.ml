open Format
open Syntax
open Support.Error
open Support.Pervasive
open Mpi

(* ------------------------   EVALUATION  ------------------------ *)

exception NoRuleApplies

let size = comm_size comm_world

let rank = comm_rank comm_world

let counter = ref 0

let newposition () =
  let value = !counter in
    counter := value + 1;
    value mod size

let isnumeric ctx t = match t with
    TmBool _ | TmInt _ | TmFloat _ -> true
  | _ -> false

let rec isval ctx t = match t with
    TmTAbs(_,_,_,_) -> true
  | TmBool(_,_)  -> true
  | TmAbs(_,_,_,_) -> true
  | TmUnit(_)  -> true
  | TmNil(_,_) -> true
  | TmCons(_,v1,v2) -> if (isval ctx v1) && (isval ctx v2) then true else false
  | TmAt(_,v1,_) when isnumeric ctx v1 -> true
  | _ -> false

let rec eval1 ctx t = match t with
    TmTApp(fi,TmTAbs(_,x,_,t11),tyT2) ->
      tytermSubstTop tyT2 t11
  | TmTApp(fi,t1,tyT2) ->
      let t1' = eval1 ctx t1 in
      TmTApp(fi, t1', tyT2)
  | TmApp(fi,TmAbs(_,x,tyT11,t12),v2) when isval ctx v2 ->
      termSubstTop v2 t12
  | TmApp(fi,v1,t2) when isval ctx v1 ->
      let t2' = eval1 ctx t2 in
      TmApp(fi, v1, t2')
  | TmApp(fi,t1,t2) ->
      let t1' = eval1 ctx t1 in
      TmApp(fi, t1', t2)
  | TmIf(_,TmBool(_,true),t2,t3) ->
      t2
  | TmIf(_,TmBool(_,false),t2,t3) ->
      t3
  | TmIf(fi,t1,t2,t3) ->
      let t1' = eval1 ctx t1 in
      TmIf(fi, t1', t2, t3)
  | TmVar(fi,n,_) ->
      (match getbinding fi ctx n with
          TmAbbBind(t,_) -> t 
        | _ -> raise NoRuleApplies)
  | TmFix(fi,v1) as t when isval ctx v1 ->
      (match v1 with
         TmAbs(_,_,_,t12) -> termSubstTop t t12
       | _ -> raise NoRuleApplies)
  | TmFix(fi,t1) ->
      let t1' = eval1 ctx t1
      in TmFix(fi,t1')
  | TmUnary(fi, op, TmAt(_, TmFloat(ffi, f1), p)) -> 
  if (p = rank) then (
    match op with
      Positive _ -> TmAt(fi, TmFloat(ffi, f1), p)
    | Negative _ -> TmAt(fi, TmFloat(ffi, -.f1), p)
    | Succ _ -> TmAt(fi, TmFloat(ffi, f1 +. 1.), p)
    | Pred _ -> TmAt(fi, TmFloat(ffi, f1 -. 1.), p)
  ) else TmAt(fi, TmFloat(ffi, 0.), p)
  | TmUnary(fi, op, TmAt(_, TmInt(ifi, i1), p)) -> 
  if (p = rank) then (
    match op with
      Positive _ -> TmAt(fi, TmInt(ifi, i1), p)
    | Negative _ -> TmAt(fi, TmInt(ifi, -i1), p)
    | Succ _ -> TmAt(fi, TmInt(ifi, succ i1), p)
    | Pred _ -> TmAt(fi, TmInt(ifi, pred i1), p)
  ) else TmAt(fi, TmInt(ifi, 0), p)
  | TmUnary(fi, op, t) -> TmUnary(fi, op, eval1 ctx t)
  | TmBinary(fi, op, TmAt(_, TmFloat(_, f1), p1), TmAt(_, TmFloat(_, f2), p2)) -> 
    if (p1 = rank) then (
      let lhs = f1 in
      let rhs = if p2 = rank then f2 else receive p2 0 comm_world in
      match op with
        Plus(_) -> TmAt(fi, TmFloat(fi, lhs +. rhs), p1)
      | Minus(_) -> TmAt(fi, TmFloat(fi, lhs -. rhs), p1)
      | Times(_) -> TmAt(fi, TmFloat(fi, lhs *. rhs), p1)
      | Over(_) -> TmAt(fi, TmFloat(fi, lhs /. rhs), p1)
      | GT(_) -> TmAt(fi, TmBool(fi, lhs > rhs), p1)
      | EQ(_) -> TmAt(fi, TmBool(fi, lhs = rhs), p1)
      | LT(_) -> TmAt(fi, TmBool(fi, lhs < rhs), p1)
    ) else (
      if (p2 = rank) then send f2 p1 0 comm_world else ();
      match op with
        Plus _ | Minus _ | Times _ | Over _ -> TmAt(fi, TmFloat(fi, 0.), p1)
      | GT _ | EQ _ | LT _  -> TmAt(fi, TmBool(fi, true), p1)
    )
  | TmBinary (fi, op, TmAt(_, TmInt(_, i1), p1),TmAt(_, TmInt(_, i2), p2)) -> 
    if p1 = rank then (
      let lhs = i1 in
      let rhs = if p2 = rank then i2 else receive p2 0 comm_world in
      match op with
        Plus(_) -> TmAt(fi, TmInt(fi, lhs + rhs), p1)
      | Minus(_) -> TmAt(fi, TmInt(fi, lhs - rhs), p1)
      | Times(_) -> TmAt(fi, TmInt(fi, lhs * rhs), p1)
      | Over(_) -> TmAt(fi, TmInt(fi, lhs / rhs), p1)
      | GT(_) -> TmAt(fi, TmBool(fi, lhs > rhs), p1)
      | EQ(_) -> TmAt(fi, TmBool(fi, lhs = rhs), p1)
      | LT(_) -> TmAt(fi, TmBool(fi, lhs < rhs), p1)
    ) else (
      if p2 = rank then send i2 p1 0 comm_world else ();
      match op with
        Plus _ | Minus _ | Times _ | Over _ -> TmAt(fi, TmInt(fi, 0), p1)
      | GT _ | EQ _ | LT _  -> TmAt(fi, TmBool(fi, true), p1)
    )
  | TmBinary(fi,op,(TmAt(_,TmFloat(_,i1),_) as t1),t2) ->
      let t2' = eval1 ctx t2 in
      TmBinary(fi,op,t1,t2')
  | TmBinary(fi,op,(TmAt(_,TmInt(_,i1),_) as t1),t2) ->
      let t2' = eval1 ctx t2 in
      TmBinary(fi,op,t1,t2')
  | TmBinary(fi,op,t1,t2) ->
      let t1' = eval1 ctx t1 in
      TmBinary(fi,op,t1',t2)
  | TmCons(fi,v1,t2) when isval ctx v1-> 
      let t2' = eval1 ctx t2 in
      TmCons(fi,v1,t2')
  | TmCons(fi,t1,t2) ->
      let t1' = eval1 ctx t1 in
      TmCons(fi,t1',t2)
  | TmIsnil(fi,TmNil(_,_)) -> TmBool(fi,true)
  | TmIsnil(fi,(TmCons(_,v11,v12) as v1)) when isval ctx v1 -> TmBool(fi,false)
  | TmIsnil(fi,t1) ->
      let t1' = eval1 ctx t1 in
      TmIsnil(fi,t1')
  | TmHead(fi,(TmCons(_,v11,v12) as v1)) when isval ctx v1 -> v11
  | TmHead(fi,t1) ->
      let t1' = eval1 ctx t1 in
      TmHead(fi,t1')
  | TmTail(fi,(TmCons(_,v11,v12) as v1)) when isval ctx v1 -> v12
  | TmTail(fi,t1) ->
      let t1' = eval1 ctx t1 in
      TmTail(fi,t1')
  | TmAt(fi, TmAt(_, t1, p1), p2) ->
    if rank = p1 then
      (send t1 p2 0 comm_world; TmAt(fi,t1,p2))
    else if rank = p2 then
      let t1' = receive p1 0 comm_world in TmAt(fi,t1',p2)
    else TmAt(fi,t1,p2)
  | TmAt(fi,t1,dis) when (not (isnumeric ctx t1)) ->
      let t1' = eval1 ctx t1 in
      TmAt(fi,t1',dis)
  | TmBool(fi, b1) as b ->
      let position = newposition () in
      if position = rank then TmAt(fi, b, position) else TmAt(fi, TmBool(fi, true), position)
  | TmInt(fi, i1) as i ->
      let position = newposition () in
      if position = rank then TmAt(fi, i, position) else TmAt(fi, TmInt(fi, 0), position)
  | TmFloat(fi, f1) as f ->
      let position = newposition () in
      if position = rank then TmAt(fi, f, position) else TmAt(fi, TmFloat(fi, 0.), position)
  | _ -> 
      raise NoRuleApplies

let rec eval ctx t =
  try let t' = eval1 ctx t
      in eval ctx t'
  with NoRuleApplies -> t

(* ------------------------   SUBTYPING  ------------------------ *)

let promote ctx t = match t with
   TyVar(i,_) ->
     (match getbinding dummyinfo ctx i with
         TyVarBind(tyT) -> tyT
       | _ -> raise NoRuleApplies)
 | _ -> raise NoRuleApplies

let istyabb ctx i = 
  match getbinding dummyinfo ctx i with
    TyAbbBind(tyT) -> true
  | _ -> false

let gettyabb ctx i = 
  match getbinding dummyinfo ctx i with
    TyAbbBind(tyT) -> tyT
  | _ -> raise NoRuleApplies

let rec computety ctx tyT = match tyT with
    TyVar(i,_) when istyabb ctx i -> gettyabb ctx i
  | _ -> raise NoRuleApplies

let rec simplifyty ctx tyT =
  try
    let tyT' = computety ctx tyT in
    simplifyty ctx tyT' 
  with NoRuleApplies -> tyT

let rec lcst ctx tyS =
  let tyS = simplifyty ctx tyS in
  try lcst ctx (promote ctx tyS)
  with NoRuleApplies -> tyS

let rec tyeqv ctx tyS tyT =
  let tyS = simplifyty ctx tyS in
  let tyT = simplifyty ctx tyT in
  match (tyS,tyT) with
    (TyArr(tyS1,tyS2),TyArr(tyT1,tyT2)) ->
       (tyeqv ctx tyS1 tyT1) && (tyeqv ctx tyS2 tyT2)
  | (TyBool,TyBool) -> true
  | (TyInt,TyInt) -> true
  | (TyUnit,TyUnit) -> true
  | (TyId(b1),TyId(b2)) -> b1=b2
  | (TyVar(i,_), _) when istyabb ctx i ->
      tyeqv ctx (gettyabb ctx i) tyT
  | (_, TyVar(i,_)) when istyabb ctx i ->
      tyeqv ctx tyS (gettyabb ctx i)
  | (TyVar(i,_),TyVar(j,_)) -> i=j
  | (TyAll(tyX1,tyS1,tyS2),TyAll(_,tyT1,tyT2)) ->
       let ctx1 = addname ctx tyX1 in
       tyeqv ctx tyS1 tyT1 && tyeqv ctx1 tyS2 tyT2
  | (TyFloat,TyFloat) -> true
  | (TyTop,TyTop) -> true
  | (TyAt(tyT1,dis1),TyAt(tyT2,dis2)) -> (tyeqv ctx tyT1 tyT2) && (dis1=dis2)
  | _ -> false

let rec subtype ctx tyS tyT =
   tyeqv ctx tyS tyT ||
   let tyS = simplifyty ctx tyS in
   let tyT = simplifyty ctx tyT in
   match (tyS,tyT) with
     (_,TyTop) -> 
       true
   | (TyVar(_,_),_) -> subtype ctx (promote ctx tyS) tyT
   | (TyAll(tyX1,tyS1,tyS2),TyAll(_,tyT1,tyT2)) ->
        (subtype ctx tyS1 tyT1 && subtype ctx tyT1 tyS1) &&
        let ctx1 = addbinding ctx tyX1 (TyVarBind(tyT1)) in
        subtype ctx1 tyS2 tyT2
   | (TyArr(tyS1,tyS2),TyArr(tyT1,tyT2)) ->
       (subtype ctx tyT1 tyS1) && (subtype ctx tyS2 tyT2)
   | (TyList(tyT1), TyList(tyT2)) ->
        subtype ctx tyT1 tyT2
   | (TyAt(tyT1,dis1), tyT2) -> (subtype ctx tyT1 tyT2)
   | (_,_) -> 
       false

let rec join ctx tyS tyT =
  if subtype ctx tyS tyT then tyT else 
  if subtype ctx tyT tyS then tyS else
  let tyS = simplifyty ctx tyS in
  let tyT = simplifyty ctx tyT in
  match (tyS,tyT) with
    (TyAll(tyX,tyS1,tyS2),TyAll(_,tyT1,tyT2)) ->
      if not(subtype ctx tyS1 tyT1 && subtype ctx tyT1 tyS1) then TyTop
      else 
        let ctx' = addbinding ctx tyX (TyVarBind(tyT1)) in
        TyAll(tyX,tyS1,join ctx' tyT1 tyT2)
  | (TyArr(tyS1,tyS2),TyArr(tyT1,tyT2)) ->
      (try TyArr(meet ctx tyS1 tyT1, join ctx tyS2 tyT2)
        with Not_found -> TyTop)
  | (TyAt(tyT1,dis1),TyAt(tyT2,dis2)) -> 
        if tyeqv ctx tyT1 tyT2 
        then ( if dis1==dis2 then TyAt(tyT1,dis1) else TyAt(tyT1,0) )
        else TyTop
  | _ -> 
      TyTop

and meet ctx tyS tyT =
  if subtype ctx tyS tyT then tyS else 
  if subtype ctx tyT tyS then tyT else 
  let tyS = simplifyty ctx tyS in
  let tyT = simplifyty ctx tyT in
  match (tyS,tyT) with
    (TyAll(tyX,tyS1,tyS2),TyAll(_,tyT1,tyT2)) ->
      if not(subtype ctx tyS1 tyT1 && subtype ctx tyT1 tyS1) then
        raise Not_found
      else 
        let ctx' = addbinding ctx tyX (TyVarBind(tyT1)) in
        TyAll(tyX,tyS1,meet ctx' tyT1 tyT2)
  | (TyArr(tyS1,tyS2),TyArr(tyT1,tyT2)) ->
      TyArr(join ctx tyS1 tyT1, meet ctx tyS2 tyT2)
  | _ -> 
      raise Not_found

(* ------------------------   TYPING  ------------------------ *)

let rec typeof ctx t =
  match t with
    TmTAbs(fi,tyX,tyT1,t2) ->
      let ctx = addbinding ctx tyX (TyVarBind(tyT1)) in
      let tyT2 = typeof ctx t2 in
      TyAll(tyX,tyT1,tyT2)
  | TmTApp(fi,t1,tyT2) ->
      let tyT1 = typeof ctx t1 in
      (match lcst ctx tyT1 with
           TyAll(_,tyT11,tyT12) ->
             if not(subtype ctx tyT2 tyT11) then
                  error fi "type parameter type mismatch";
             typeSubstTop tyT2 tyT12
         | _ -> error fi "universal type expected")
  | TmVar(fi,i,_) -> getTypeFromContext fi ctx i
  | TmAbs(fi,x,tyT1,t2) ->
      let ctx' = addbinding ctx x (VarBind(tyT1)) in
      let tyT2 = typeof ctx' t2 in
      TyArr(tyT1, typeShift (-1) tyT2)
  | TmApp(fi,t1,t2) ->
      let tyT1 = typeof ctx t1 in
      let tyT2 = typeof ctx t2 in
      (match lcst ctx tyT1 with
          TyArr(tyT11,tyT12) ->
            if subtype ctx tyT2 tyT11 then tyT12
            else error fi "parameter type mismatch"
        | _ -> error fi "arrow type expected")
  | TmBool(fi,_) -> TyBool
  | TmIf(fi,t1,t2,t3) ->
      if subtype ctx (typeof ctx t1) TyBool then
        join ctx (typeof ctx t2) (typeof ctx t3)
      else error fi "guard of conditional not a boolean"
  | TmUnit _ -> TyUnit
  | TmInt _ -> TyInt
  | TmFloat _ -> TyFloat
  | TmUnary(fi,op,t1) ->
    let tyT1 = typeof ctx t1 in
      if subtype ctx tyT1 TyFloat || subtype ctx tyT1 TyInt then tyT1 else error fi "argument of positive/negative/pred/succ is not a number"
  | TmBinary(fi,op,t1,t2) -> (
    let tyT1 = typeof ctx t1 in
    let tyT2 = typeof ctx t2 in
    let tyB = join ctx tyT1 tyT2 in (
      match op with
        Plus _ | Minus _ | Times _ | Over _ ->
          if subtype ctx tyB TyFloat || subtype ctx tyB TyInt
            then tyB 
            else error fi "argument of plus/minus/times/over is not a number"
      | EQ _ | GT _ | LT _ ->
          if subtype ctx tyB TyFloat || subtype ctx tyB TyInt
            then TyBool
            else error fi "argument of comparison operator (eq, gt, lt, ge, le) is not a number"
    )
  )
  | TmFix(fi, t1) ->
      let tyT1 = typeof ctx t1 in
      (match lcst ctx tyT1 with
           TyArr(tyT11,tyT12) ->
             if subtype ctx tyT12 tyT11 then tyT12
             else error fi "result of body not compatible with domain"
         | _ -> error fi "arrow type expected")
  | TmNil(fi,tyT1) -> TyList(tyT1)
  | TmCons(fi,t1,t2) ->
      let tyT1 = typeof ctx t1 in
      let tyT2 = typeof ctx t2 in
      (match t2 with
          TmNil(_,ty) -> if (subtype ctx tyT1 ty) then TyList(tyT1) else error fi "list type is not unique"
        | _ -> (match tyT2 with
            TyList(tyT) -> 
              let tyB = join ctx tyT1 tyT in
              if (tyeqv ctx tyB TyTop) then error fi "list type is not unique" else TyList(tyB)
          | _ -> error fi "not a list")

      )
  | TmIsnil(fi,t1) ->
      let tyT1 = typeof ctx t1 in
      (match tyT1 with
        TyList(tyT) -> TyBool
      | _ -> error fi "not a list")
  | TmHead(fi,t1) ->
      let tyT1 = typeof ctx t1 in
      (match tyT1 with
        TyList(tyT) -> tyT
      | _ -> error fi "not a list")
  | TmTail(fi,t1) ->
      let tyT1 = typeof ctx t1 in
      (match tyT1 with
        TyList(tyT) -> TyList(tyT)
      | _ -> error fi "not a list")
  | TmAt(fi,t1,dis) ->
      if (dis < size) then (
      let rec f tyT = (
        match tyT with
          TyAt(tyT2,_) -> f tyT2
        | _ -> tyT
      ) in
      let tyT1 = typeof ctx t1 in TyAt((f tyT1),dis)
      ) else error fi "exceeded maximum rank!"

let evalbinding ctx b = match b with
    TmAbbBind(t,tyT) ->
      let t' = eval ctx t in 
      TmAbbBind(t',tyT)
  | bind -> bind
