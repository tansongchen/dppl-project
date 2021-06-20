(* 
   The lexical analyzer: lexer.ml is generated automatically
   from lexer.mll.
   
   The only modification commonly needed here is adding new keywords to the 
   list of reserved words at the top.  
*)

{
open Support.Error

let reservedWords = [
  (* Keywords *)
  ("lambda", fun i -> Parser.LAMBDA i);
  ("if", fun i -> Parser.IF i);
  ("then", fun i -> Parser.THEN i);
  ("else", fun i -> Parser.ELSE i);
  ("fix", fun i -> Parser.FIX i);
  ("head", fun i -> Parser.HEAD i);
  ("tail", fun i -> Parser.TAIL i);
  ("isnil", fun i -> Parser.ISNIL i);

  ("unit", fun i -> Parser.UNIT i);
  ("true", fun i -> Parser.TRUE i);
  ("false", fun i -> Parser.FALSE i);

  ("All", fun i -> Parser.ALL i);
  ("Top", fun i -> Parser.TOP i);
  ("Unit", fun i -> Parser.UUNIT i);
  ("Bool", fun i -> Parser.UBOOL i);
  ("Int", fun i -> Parser.UINT i);
  ("Float", fun i -> Parser.UFLOAT i);
  ("List", fun i -> Parser.ULIST i);
  
  (* Symbols *)
  ("!", fun i -> Parser.EXCLAMATION i);
  ("\"", fun i -> Parser.QUOTATION i);
  ("#", fun i -> Parser.HASH i);
  ("$", fun i -> Parser.DOLLAR i);
  ("%", fun i -> Parser.PERCENT i);
  ("&", fun i -> Parser.AMPERSAND i);
  ("'", fun i -> Parser.APOSTROPHE i);
  ("(", fun i -> Parser.LPARENTHESIS i);
  (")", fun i -> Parser.RPARENTHESIS i);
  ("*", fun i -> Parser.ASTERISK i);
  ("+", fun i -> Parser.PLUS i);
  (",", fun i -> Parser.COMMA i);
  ("-", fun i -> Parser.MINUS i);
  (".", fun i -> Parser.DOT i);
  ("/", fun i -> Parser.SLASH i);

  (":", fun i -> Parser.COLON i);
  (";", fun i -> Parser.SEMICOLON i);
  ("<", fun i -> Parser.LT i);
  ("=", fun i -> Parser.EQ i);
  (">", fun i -> Parser.GT i);
  ("?", fun i -> Parser.QUESTION i);
  ("@", fun i -> Parser.AT i);
  ("[", fun i -> Parser.LSQUARE i);
  ("]", fun i -> Parser.RSQUARE i);
  ("^", fun i -> Parser.CARET i);
  ("_", fun i -> Parser.UNDERSCORE i);
  ("`", fun i -> Parser.GRAVE i);
  ("{", fun i -> Parser.LCURLY i);
  ("|", fun i -> Parser.VERTICALBAR i);
  ("}", fun i -> Parser.RCURLY i);
  ("~", fun i -> Parser.TILDE i);

  (* Special compound symbols: *)
  ("++", fun i -> Parser.PLUSPLUS i);
  ("--", fun i -> Parser.MINUSMINUS i);
  ("::", fun i -> Parser.COLONCOLON i);
  ("<:", fun i -> Parser.LTCOLON i);
  ("<=", fun i -> Parser.LE i);
  (">=", fun i -> Parser.GE i);
  ("->", fun i -> Parser.ARROW i);
]

(* Support functions *)

type buildfun = info -> Parser.token
let (symbolTable : (string, buildfun) Hashtbl.t) = Hashtbl.create 1024
let _ =
  List.iter (fun (str, f) -> Hashtbl.add symbolTable str f) reservedWords

let createID i str =
  try (Hashtbl.find symbolTable str) i
  with _ ->
    let initial = String.get str 0 in
    if initial >= 'A' && initial <= 'Z' then
       Parser.UCID {i=i;v=str}
    else 
       Parser.LCID {i=i;v=str}

let lineno   = ref 1
and depth    = ref 0
and start    = ref 0

and filename = ref ""
and startLex = ref dummyinfo

let create inFile stream =
  if not (Filename.is_implicit inFile) then filename := inFile
  else filename := Filename.concat (Sys.getcwd()) inFile;
  lineno := 1; start := 0; Lexing.from_channel stream

let newline lexbuf = incr lineno; start := (Lexing.lexeme_start lexbuf)

let info lexbuf =
  createInfo (!filename) (!lineno) (Lexing.lexeme_start lexbuf - !start)

let text = Lexing.lexeme

let stringBuffer = ref (Bytes.create 2048)
let stringEnd = ref 0

let resetStr () = stringEnd := 0

let addStr ch =
  let x = !stringEnd in
  let buffer = !stringBuffer
in
  if x = Bytes.length buffer then
    begin
      let newBuffer = Bytes.create (x*2) in
      Bytes.blit buffer 0 newBuffer 0 x;
      Bytes.set newBuffer x ch;
      stringBuffer := newBuffer;
      stringEnd := x+1
    end
  else
    begin
      Bytes.set buffer x ch;
      stringEnd := x+1
    end

let getStr () = Bytes.sub_string (!stringBuffer) 0 (!stringEnd)

let extractLineno yytext offset =
  int_of_string (String.sub yytext offset (String.length yytext - offset))
}


(* The main body of the lexical analyzer *)

rule main = parse
  [' ' '\009' '\012']+     { main lexbuf }

| [' ' '\009' '\012']*"\n" { newline lexbuf; main lexbuf }

| "*/" { error (info lexbuf) "Unmatched end of comment" }

| "/*" { depth := 1; startLex := info lexbuf; comment lexbuf; main lexbuf }

| '-'?['0'-'9']+
    { Parser.INT{i=info lexbuf; v=int_of_string (text lexbuf)} }

| '-'?['0'-'9']+ '.' ['0'-'9']*
    { Parser.FLOAT{i=info lexbuf; v=float_of_string (text lexbuf)} }

| ['A'-'Z' 'a'-'z' '_']
  ['A'-'Z' 'a'-'z' '_' '0'-'9' '\'']*
    { createID (info lexbuf) (text lexbuf) }

| ":=" | "<:" | "<-" | "->" | "=>" | "==>"
| "{|" | "|}" | "<|" | "|>" | "[|" | "|]" | "=="
    { createID (info lexbuf) (text lexbuf) }

| ['~' '%' '\\' '+' '-' '&' '|' ':' '@' '`' '$']+
    { createID (info lexbuf) (text lexbuf) }

| ['*' '#' '/' '!' '?' '^' '(' ')' '{' '}' '[' ']' '<' '>' '.' ';' '_' ','
   '=' '\'']
    { createID (info lexbuf) (text lexbuf) }

| eof { Parser.EOF(info lexbuf) }

| _  { error (info lexbuf) "Illegal character" }

and comment = parse
  "/*"
    { depth := succ !depth; comment lexbuf }
| "*/"
    { depth := pred !depth; if !depth > 0 then comment lexbuf }
| eof
    { error (!startLex) "Comment not terminated" }
| [^ '\n']
    { comment lexbuf }
| "\n"
    { newline lexbuf; comment lexbuf }
