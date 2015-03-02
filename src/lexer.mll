(*
  Simplet

  Copyright (C) 2014 Pascal BENCHIMOL (fistname.lastname at polytechnique.edu)
  
  This library is free software; you can redistribute it and/or
  modify it under the terms of the GNU Lesser General Public
  License as published by the Free Software Foundation; either
  version 2.1 of the License, or (at your option) any later version.
  
  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  Lesser General Public License for more details.
  
  You should have received a copy of the GNU Lesser General Public
  License along with this library; if not, write to the Free Software
  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

*)

{
  open LinearProg
  open Tokens
  open Lexing

  exception Error of string
      
  type semiring = MaxPlus | MinPlus



  let semiring = ref MaxPlus

  let numeric = ref "ocaml_int"

  let var_names = Hashtbl.create 53
(*
  let _ =
    List.iter (fun (kwd, tok) -> Hashtbl.add var_names kwd tok)
              [ "x", 1; "y",2; "z",3;]
*)
  let add_var s nb_var = 
    try let _ = Hashtbl.find var_names s  in
        raise (Error ("Error: Var name '"^s^"' used twice.\n"))
    with Not_found -> Hashtbl.add var_names s nb_var

(*
  let lex_line_nb lexbuf =
    let pos = Lexing.lexeme_start_p lexbuf in
    pos.pos_lnum
*)

let print_position  lexbuf =
  let pos = lexbuf.lex_start_p in
  Printf.sprintf "line %d, char %d" 
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol )

}



let spaces = [' ' '\t']
let ident = [ 'a'-'z' 'A'-'Z' '_'][ 'a'-'z' 'A'-'Z' '_' '0'-'9']* 
rule line = parse
| ([^'\n']* '\n') as line
    {Lexing.new_line lexbuf; line }
| eof
    { exit 0 }

and header = parse
| spaces+  {header lexbuf}
| '\n' | '#' [^ '\n']* '\n' { Lexing.new_line lexbuf; header lexbuf}
| "vars" spaces* ':'  { parse_vars 0 lexbuf; header lexbuf }
| "semiring" spaces* ':' { parse_semiring lexbuf; header lexbuf }
| "numeric" spaces* ':' { parse_numeric lexbuf; header lexbuf}
| "lp" spaces* ':' { (numeric,var_names) }
| _  { raise (Error (Printf.sprintf "At %s: unexpected character: %s \n" (print_position lexbuf) (Lexing.lexeme lexbuf))) }

and parse_vars nb_var = parse
  | spaces* (ident as s) spaces* {add_var s nb_var }
  | spaces* (ident as s) spaces* ','  { add_var s nb_var; parse_vars (nb_var + 1) lexbuf }

(*
and parse_initial_basic_point = parse
| spaces+  {header lexbuf}
| '\n' | '#' [^ '\n']* '\n' { Lexing.new_line lexbuf; header lexbuf}
| "basic point" spaces* ':' {true}
| _ {false}
*)

and parse_semiring  = parse
  | spaces+ { parse_semiring lexbuf }
  | "maxplus" { semiring := MaxPlus }
  | "minplus" { semiring := MinPlus }
  | _  { raise (Error (Printf.sprintf "At %s: expected 'maxplus' or 'minplus' \n" (print_position lexbuf) )) }


and parse_numeric = parse
  | spaces+ { parse_numeric lexbuf}
  | "int" {  numeric := "ocaml_int" }
  | "float" { numeric := "ocaml_float" }
  | "big_int" {  numeric := "ocaml_big_int" }
  | "big_rational" { numeric := "ocaml_big_rat" }
  | _  { raise (Error (Printf.sprintf "At %s: expected 'int' or 'float' or 'big_int' or 'big_rational'. \n" (print_position lexbuf) )) }

and token = parse
| spaces+ {token lexbuf}
| '\n' | '#' [^ '\n']* '\n' {Lexing.new_line lexbuf; token lexbuf}
| eof { EOF}

| "maximize" {MAXIMIZE}
| "minimize" {MINIMIZE}
| ['0'-'9']+('.'['0'-'9']*)? as s
    { NUM s }
| "max"  { MAX}
(*
    { match !semiring with
      | MaxPlus -> MAX
      | MinPlus ->  raise (Error (Printf.sprintf "At %s: 'max' not allowed.\n" (print_position lexbuf)))
  }
| "min" 
    { match !semiring with
      | MaxPlus -> MAX
      | MinPlus ->  raise (Error (Printf.sprintf "At %s: 'min' not allowed.\n" (print_position lexbuf)))
  }
*)
| ident as s
    { try
        let var_index = Hashtbl.find var_names s in
        VAR (Var var_index)
      with Not_found ->  raise (Error (Printf.sprintf "At %s: unexpected identifier: '%s'.\n" (print_position lexbuf) s))
}
| "<=" 
   { match !semiring with
     |MaxPlus -> LEQ
     |MinPlus -> GEQ
   }
| ">=" 
   { match !semiring with
     |MaxPlus -> GEQ
     |MinPlus -> LEQ
   }

| '=' { EQ}
| '+' { PLUS }
| '-' { MINUS }
| '*' { TIMES }
| '/' { DIV }
| '(' { LPAREN }
| ')' { RPAREN }
| ';' { SEMICOLON }
| ',' { COMMA }
| "basic point" {BASIC_POINT}
| _  { raise (Error (Printf.sprintf "At %s: unexpected character: %s \n" (print_position lexbuf) (Lexing.lexeme lexbuf))) }
  (*  { raise (Error (Printf.sprintf " unexpected character: \n")) }*)

