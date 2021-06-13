(* module Syntax: syntax trees and associated support functions *)

open Support.Pervasive
open Support.Error

(* Data type definitions *)

type operator =
    Plus of info
  | Minus of info
  | Times of info
  | Divide of info
  | LT of info
  | GT of info
  | EQ of info

type ty =
    TyTop
  | TyVar of int * int
  | TyString
  | TyAll of string * ty * ty
  | TySome of string * ty * ty
  | TyArr of ty * ty
  | TyRecord of (string * ty) list
  | TyBool
  | TyInt
  | TyUnit
  | TyId of string
  | TyFloat
  | TyList of ty
  | TyAt of ty * int

type term =
    TmAscribe of info * term * ty
  | TmString of info * string
  | TmTAbs of info * string * ty * term
  | TmTApp of info * term * ty
  | TmPack of info * ty * term * ty
  | TmUnpack of info * string * string * term * term
  | TmVar of info * int * int
  | TmAbs of info * string * ty * term
  | TmApp of info * term * term
  | TmRecord of info * (string * term) list
  | TmProj of info * term * string
  | TmTrue of info
  | TmFalse of info
  | TmIf of info * term * term * term
  | TmInt of info * int
  | TmIsZero of info * term
  | TmUnit of info
  | TmFloat of info * float
  | TmTimesfloat of info * term * term
  | TmLet of info * string * term * term
  | TmInert of info * ty
  | TmFix of info * term
  | TmBinary of info * operator * term * term
  | TmPlus of info * term * term
  | TmGt of info * term * term
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
    Import of string
  | Eval of info * term
  | Bind of info * string * binding
  | SomeBind of info * string * string * term

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

