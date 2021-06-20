/*  
 *  Yacc grammar for the parser.  The files parser.mli and parser.ml
 *  are generated automatically from parser.mly.
 */

%{
open Support.Error
open Support.Pervasive
open Syntax
%}

/* ---------------------------------------------------------------------- */
/* Preliminaries */

/* We first list all the tokens mentioned in the parsing rules
   below.  The names of the tokens are common to the parser and the
   generated lexical analyzer.  Each token is annotated with the type
   of data that it carries; normally, this is just file information
   (which is used by the parser to annotate the abstract syntax trees
   that it constructs), but sometimes -- in the case of identifiers and
   constant values -- more information is provided.
 */

/* Keyword tokens */
%token <Support.Error.info> LAMBDA
%token <Support.Error.info> IF
%token <Support.Error.info> THEN
%token <Support.Error.info> ELSE
%token <Support.Error.info> FIX
%token <Support.Error.info> HEAD
%token <Support.Error.info> TAIL
%token <Support.Error.info> ISNIL
%token <Support.Error.info> UNIT
%token <Support.Error.info> TRUE
%token <Support.Error.info> FALSE
%token <Support.Error.info> ALL
%token <Support.Error.info> TOP
%token <Support.Error.info> UUNIT
%token <Support.Error.info> UBOOL
%token <Support.Error.info> UINT
%token <Support.Error.info> UFLOAT
%token <Support.Error.info> ULIST

/* Identifier and constant value tokens */
%token <string Support.Error.withinfo> UCID  /* uppercase-initial */
%token <string Support.Error.withinfo> LCID  /* lowercase/symbolic-initial */
%token <int Support.Error.withinfo> INT
%token <float Support.Error.withinfo> FLOAT

/* Symbolic tokens */
%token <Support.Error.info> ARROW
%token <Support.Error.info> EXCLAMATION
%token <Support.Error.info> QUOTATION
%token <Support.Error.info> HASH
%token <Support.Error.info> DOLLAR
%token <Support.Error.info> PERCENT
%token <Support.Error.info> AMPERSAND
%token <Support.Error.info> APOSTROPHE
%token <Support.Error.info> LPARENTHESIS
%token <Support.Error.info> RPARENTHESIS
%token <Support.Error.info> ASTERISK
%token <Support.Error.info> PLUS
%token <Support.Error.info> COMMA
%token <Support.Error.info> MINUS
%token <Support.Error.info> DOT
%token <Support.Error.info> SLASH

%token <Support.Error.info> COLON
%token <Support.Error.info> SEMICOLON
%token <Support.Error.info> LT
%token <Support.Error.info> EQ
%token <Support.Error.info> GT
%token <Support.Error.info> QUESTION
%token <Support.Error.info> AT
%token <Support.Error.info> LSQUARE
%token <Support.Error.info> RSQUARE
%token <Support.Error.info> CARET
%token <Support.Error.info> UNDERSCORE
%token <Support.Error.info> GRAVE
%token <Support.Error.info> LCURLY
%token <Support.Error.info> VERTICALBAR
%token <Support.Error.info> RCURLY
%token <Support.Error.info> TILDE

%token <Support.Error.info> PLUSPLUS
%token <Support.Error.info> MINUSMINUS
%token <Support.Error.info> COLONCOLON
%token <Support.Error.info> LTCOLON
%token <Support.Error.info> LE
%token <Support.Error.info> GE
%token <Support.Error.info> ARROW

%token <Support.Error.info> EOF

/* ---------------------------------------------------------------------- */
/* The starting production of the generated parser is the syntactic class
   toplevel.  The type that is returned when a toplevel is recognized is
     Syntax.context -> (Syntax.command list * Syntax.context) 
   that is, the parser returns to the user program a function that,
   when given a naming context, returns a fully parsed list of
   Syntax.commands and the new naming context that results when
   all the names bound in these commands are defined.

   All of the syntactic productions in the parser follow the same pattern:
   they take a context as argument and return a fully parsed abstract
   syntax tree (and, if they involve any constructs that bind variables
   in some following phrase, a new context).
   
*/

%start toplevel
%type < Syntax.context -> (Syntax.command list * Syntax.context) > toplevel
%left PLUS MINUS
%%

/* ---------------------------------------------------------------------- */
/* Main body of the parser definition */

/* The top level of a file is a sequence of commands, each terminated
   by a SEMICOLONcolon. */
toplevel :
    EOF
      { fun ctx -> [], ctx }
  | Command SEMICOLON toplevel
      { fun ctx ->
          let cmd, ctx = $1 ctx in
          let cmds, ctx = $3 ctx in
          cmd :: cmds, ctx }

/* A top-level command */
Command :
    Term 
      { fun ctx -> (let t = $1 ctx in Eval(tmInfo t, t)),ctx }
  | UCID TypeBinder
      { fun ctx -> ((Bind($1.i, $1.v, $2 ctx)), addname ctx $1.v) }
  | LCID ValueBinder
      { fun ctx -> ((Bind($1.i, $1.v, $2 ctx)), addname ctx $1.v) }

TypeBinder :
    /* empty */
      { fun ctx -> TyVarBind(TyTop) }
  | LTCOLON Type
      { fun ctx -> TyVarBind($2 ctx) }
  | TILDE Type
      { fun ctx -> TyAbbBind($2 ctx) }

/* Right-hand sides of top-level bindings */
ValueBinder :
    COLON Type
      { fun ctx -> VarBind($2 ctx)}
  | TILDE Term 
      { fun ctx -> TmAbbBind($2 ctx, None) }

/* All type expressions */
Type :
    ArrowType
      { $1 }
  | ALL UCID DOT Type
      { fun ctx ->
          let ctx1 = addname ctx $2.v in
          TyAll($2.v, TyTop, $4 ctx1) }
  | ALL UCID LTCOLON Type DOT Type
      { fun ctx ->
          let ctx1 = addname ctx $2.v in
          TyAll($2.v, $4 ctx, $6 ctx1) }

/* An "arrow type" is a sequence of atomic types separated by
   arrows. */
ArrowType :
    AtomicType ARROW ArrowType
     { fun ctx -> TyArr($1 ctx, $3 ctx) }
  | AtomicType
     { $1 }

/* Atomic types are those that never need extra parentheses */
AtomicType :
    LPARENTHESIS Type RPARENTHESIS
           { $2 }
  | TOP
      { fun ctx -> TyTop }
  | UCID 
      { fun ctx ->
          if isnamebound ctx $1.v then
            TyVar(name2index $1.i ctx $1.v, ctxlength ctx)
          else 
            TyId($1.v) }
  | UUNIT
      { fun ctx -> TyUnit }
  | UBOOL
      { fun ctx -> TyBool }
  | UINT
      { fun ctx -> TyInt }
  | UFLOAT
      { fun ctx -> TyFloat }
  | ULIST AtomicType
      { fun ctx -> TyList($2 ctx) }

Term :
    AppTerm
      { $1 }
  | LAMBDA UCID DOT Term
      { fun ctx ->
          let ctx1 = addname ctx $2.v in
          TmTAbs($1, $2.v, TyTop, $4 ctx1) }
  | LAMBDA UCID LTCOLON Type DOT Term
      { fun ctx ->
          let ctx1 = addname ctx $2.v in
          TmTAbs($1, $2.v, $4 ctx, $6 ctx1) }
  | LAMBDA LCID COLON Type DOT Term 
      { fun ctx ->
          let ctx1 = addname ctx $2.v in
          TmAbs($1, $2.v, $4 ctx, $6 ctx1) }
  | LAMBDA UNDERSCORE COLON Type DOT Term 
      { fun ctx ->
          let ctx1 = addname ctx "_" in
          TmAbs($1, "_", $4 ctx, $6 ctx1) }
  | IF Term THEN Term ELSE Term
      { fun ctx -> TmIf($1, $2 ctx, $4 ctx, $6 ctx) }

UnaryOperator :
    PLUS { fun ctx -> Positive($1) }
  | MINUS { fun ctx -> Negative($1) }
  | PLUSPLUS { fun ctx -> Succ($1) }
  | MINUSMINUS { fun ctx -> Pred($1) }

BinaryOperator :
    PLUS     { fun ctx -> Plus($1) }
  | MINUS    { fun ctx -> Minus($1) }
  | ASTERISK { fun ctx -> Times($1) }
  | SLASH    { fun ctx -> Over($1) }
  | EQ       { fun ctx -> EQ($1) }
  | GT       { fun ctx -> GT($1) }
  | LT       { fun ctx -> LT($1) }

TermSeq :
    Term 
      { $1 }
  | Term SEMICOLON TermSeq 
      { fun ctx ->
          TmApp($2, TmAbs($2, "_", TyUnit, $3 (addname ctx "_")), $1 ctx) }

AppTerm :
    AppTerm LSQUARE Type RSQUARE
      { fun ctx ->
          let t1 = $1 ctx in
          let t2 = $3 ctx in
          TmTApp(tmInfo t1,t1,t2) }
  | AppTerm AtomicTerm
      { fun ctx ->
          let e1 = $1 ctx in
          let e2 = $2 ctx in
          TmApp(tmInfo e1,e1,e2) }
  | UnaryOperator AtomicTerm
      { fun ctx -> 
            let t2 = $2 ctx in
            TmUnary(tmInfo t2, $1 ctx, $2 ctx) }
  | AppTerm BinaryOperator AtomicTerm
      { fun ctx -> 
            let t1 = $1 ctx in
            TmBinary(tmInfo t1, $2 ctx, t1, $3 ctx) }
  | AppTerm COLONCOLON AtomicTerm
      { fun ctx -> TmCons($2, $1 ctx, $3 ctx) }
  | FIX AtomicTerm
      { fun ctx ->
          TmFix($1, $2 ctx) }
  | HEAD AtomicTerm
      { fun ctx -> TmHead($1, $2 ctx) }
  | TAIL AtomicTerm
      { fun ctx -> TmTail($1, $2 ctx) }
  | ISNIL AtomicTerm
      { fun ctx -> TmIsnil($1, $2 ctx) }
  | AtomicTerm
      { $1 }

/* Atomic terms are ones that never require extra parentheses */
AtomicTerm :
    LPARENTHESIS TermSeq RPARENTHESIS
      { $2 } 
  | LCID 
      { fun ctx ->
          TmVar($1.i, name2index $1.i ctx $1.v, ctxlength ctx) }
  | UNIT
      { fun ctx -> TmUnit($1) }
  | TRUE
      { fun ctx -> TmBool($1, true) }
  | FALSE
      { fun ctx -> TmBool($1, false) }
  | INT
      { fun ctx -> TmInt($1.i, $1.v) }
  | FLOAT
      { fun ctx -> TmFloat($1.i, $1.v) }
  | AtomicTerm AT INT
      { fun ctx -> TmAt($2, $1 ctx, $3.v) }
  | AtomicType LSQUARE RSQUARE
      { fun ctx -> TmNil(dummyinfo, $1 ctx) }
  | LSQUARE AppTerm ListContent RSQUARE
      { fun ctx -> TmCons($1, $2 ctx, $3 ctx) }

ListContent :
    /* empty */
      { fun ctx -> TmNil(dummyinfo, TyTop) }
  | COMMA AppTerm ListContent
      { fun ctx -> TmCons($1, $2 ctx, $3 ctx) }
