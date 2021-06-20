(* module Syntax: syntax trees and associated support functions *)

open Support.Pervasive
open Support.Error

(* Data type definitions *)

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

type command =
    Eval of info * term
  | Bind of info * string * binding

(* Contexts *)
type context
val emptycontext : context 
val ctxlength : context -> int
val addbinding : context -> string -> binding -> context
val addname: context -> string -> context
val index2name : info -> context -> int -> string
val getbinding : info -> context -> int -> binding
val name2index : info -> context -> string -> int
val isnamebound : context -> string -> bool
val getTypeFromContext : info -> context -> int -> ty

(* Shifting and substitution *)
val termShift: int -> term -> term
val termSubstTop: term -> term -> term
val typeShift : int -> ty -> ty
val typeSubstTop: ty -> ty -> ty
val tytermSubstTop: ty -> term -> term

(* Printing *)
val printtm: context -> term -> unit
val printtm_ATerm: bool -> context -> term -> unit
val printty : context -> ty -> unit
val prbinding : context -> binding -> unit

(* Misc *)
val tmInfo: term -> info

