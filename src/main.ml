(* Module Main: The main program.  Deals with processing the command
   line, reading files, building and connecting lexers and parsers, etc. 
   
   For most experiments with the implementation, it should not be
   necessary to change this file.
*)

open Format
open Support.Pervasive
open Support.Error
open Syntax
open Core
open Mpi

let size = comm_size comm_world
let rank = comm_rank comm_world

let searchpath = ref [""]

let argDefs = [
  "-I",
      Arg.String (fun f -> searchpath := f::!searchpath),
      "Append a directory to the search path"]

let parseArgs () =
  let inFile = ref (None : string option) in
  Arg.parse argDefs
     (fun s ->
       match !inFile with
         Some(_) -> err "You must specify exactly one input file"
       | None -> inFile := Some(s))
     "";
  match !inFile with
      None -> err "You must specify an input file"
    | Some(s) -> s

let openfile infile = 
  let rec trynext l = match l with
        [] -> err ("Could not find " ^ infile)
      | (d::rest) -> 
          let name = if d = "" then infile else (d ^ "/" ^ infile) in
          try open_in name
            with Sys_error m -> trynext rest
  in trynext !searchpath

let parseFile inFile =
  let pi = openfile inFile
  in let lexbuf = Lexer.create inFile pi
  in let result =
    try Parser.toplevel Lexer.main lexbuf with Parsing.Parse_error -> 
    error (Lexer.info lexbuf) "Parse error"
in
  Parsing.clear_parser(); close_in pi; result

let checkbinding fi ctx b = match b with
    NameBind -> NameBind
  | TyVarBind(tyS) -> TyVarBind(tyS)
  | TyAbbBind(tyT) -> TyAbbBind(tyT)
  | VarBind(tyT) -> VarBind(tyT)
  | TmAbbBind(t,None) -> TmAbbBind(t, Some(typeof ctx t))
  | TmAbbBind(t,Some(tyT)) ->
     let tyT' = typeof ctx t in
     if subtype ctx tyT' tyT then TmAbbBind(t,Some(tyT))
     else error fi "Type of binding does not match declared type"

let prbindingty ctx b = match b with
    NameBind -> ()
  | TyVarBind(tyS) -> pr "<: ";printty ctx tyS
  | VarBind(tyT) -> pr ": "; printty ctx tyT 
  | TyAbbBind(tyT) -> pr ":: *"
  | TmAbbBind(t, tyT_opt) -> pr ": ";
     (match tyT_opt with
         None -> printty ctx (typeof ctx t)
       | Some(tyT) -> printty ctx tyT)


let rec fetch_print ctx t =
  match t with
    TmAt(_, _, 0) | TmNil(_, _) -> if rank = 0 then printtm_ATerm true ctx t else ()
  | TmAt(_, _, p) ->
      if rank = 0
        then let t' = receive p 0 comm_world in printtm_ATerm true ctx t'
        else if rank = p then send t 0 0 comm_world else ()
  | TmCons(_, v1, v2) -> fetch_print ctx v1; if rank = 0 then pr " :: " else (); fetch_print ctx v2;
  | _ -> if rank = 0 then printtm_ATerm true ctx t else ()

let process_command ctx cmd = 
  match cmd with
    Eval(fi,t) -> 
      (* pr "Eval start"; *)
      let tyT = typeof ctx t in
      (* pr "Type checking finished"; *)
      (* printtm_ATerm true ctx t; *)
      let t' = eval ctx t in
      (* pr "Eval finished"; *)
      (* printtm_ATerm true ctx t';  *)
      fetch_print ctx t';
      if rank = 0 then (
      print_break 1 2;
      pr ": ";
      printty ctx tyT;
      force_newline()
      ) else ();
      ctx
  | Bind(fi,x,bind) -> 
      let bind = checkbinding fi ctx bind in
      let bind' = evalbinding ctx bind in
      if rank = 0 then (
      pr x; pr " "; prbindingty ctx bind'; force_newline()
      ) else ();
      addbinding ctx x bind'

let process_file f ctx =
  let cmds,_ = parseFile f ctx in
  let g ctx c =  
    open_hvbox 0;
    let results = process_command ctx c in
    print_flush();
    results
  in
    List.fold_left g ctx cmds

let main () = 
  let inFile = parseArgs() in
  let _ = process_file inFile emptycontext in
  ()

let () = set_max_boxes 1000
let () = set_margin 67
let res = 
  Printexc.catch (fun () -> 
    try main();0 
    with Exit x -> x) 
  ()
let () = print_flush()
let () = exit res
