/**************************************************************************/
/*                                                                        */
/*  Menhir                                                                */
/*                                                                        */
/*  François Pottier, INRIA Rocquencourt                                  */
/*  Yann Régis-Gianas, PPS, Université Paris Diderot                      */
/*                                                                        */
/*  Copyright 2005-2008 Institut National de Recherche en Informatique    */
/*  et en Automatique. All rights reserved. This file is distributed      */
/*  under the terms of the Q Public License version 1.0, with the change  */
/*  described in file LICENSE.                                            */
/*                                                                        */
/**************************************************************************/



%parameter<Numeric : sig
  type t
  val zero : t
  val add : t -> t -> t
  val neg : t -> t
  val mul : t -> t -> t
  val div : t -> t -> t
  val of_string : string -> t
  val to_string : t -> string
end>

%token BASIC_POINT
%token MAXIMIZE MINIMIZE
%token <string> NUM
%token <LinearProg.col_index_t> VAR
%token MAX
%token PLUS MINUS TIMES DIV
%token LPAREN RPAREN
%token COMMA SEMICOLON
%token EQ GEQ LEQ
%token EOF


%left PLUS MINUS        /* lowest precedence */
%left TIMES DIV         /* medium precedence */
%nonassoc UMINUS        /* highest precedence */



%start < (LinearProg.col_index_t * LinearProg.sign_t * Numeric.t) list *
  ((LinearProg.col_index_t * LinearProg.sign_t * Numeric.t) list) list *
  Numeric.t list> 
  main

(*
%start <(LinearProg.col_index_t *  Numeric.t) list>  ll
%start <(LinearProg.col_index_t * LinearProg.sign_t *  Numeric.t) list>  const_eof
%start <((LinearProg.col_index_t * LinearProg.sign_t *  Numeric.t) list) list>  consts_eof*)
(* %start  <Numeric.t>  num_expr *)


%{

  open LinearProg

  

%}

%%



term:
| v = num_expr { (Affine, v) }
| v = num_expr PLUS j = VAR {(j,v) }

| j = VAR { (j, Numeric.zero) }
| j = VAR PLUS v = num_expr { (j,v) }
| j = VAR MINUS v = num_expr { let vv = Numeric.neg v in
(*Printf.printf "term  %s\n"  (Numeric.to_string vv);*)
 (j,vv) }

num_expr:
| s = NUM
    { Numeric.of_string s  }
| LPAREN e = num_expr RPAREN
    { e }
| e1 = num_expr PLUS e2 = num_expr
    { Numeric.add e1  e2 }
| e1 = num_expr MINUS e2 = num_expr
    { let ne2 = Numeric.neg e2 in Numeric.add e1 ne2 }
| e1 = num_expr TIMES e2 = num_expr
    { Numeric.mul e1  e2 }
| e1 = num_expr DIV e2 = num_expr
    { Numeric.div e1  e2 }
| MINUS e = num_expr %prec UMINUS
    { Numeric.neg e }
    


linear_form:
| t = term {(*Printf.printf "linear form, simple term\n ";*) t::[] }
| MAX LPAREN list = term_list RPAREN {(*Printf.printf "linear form, max\n ";*) list}


term_list:
| l = separated_nonempty_list(COMMA, term) { l }
(*
| t = term { t::[] }
| t = term COMMA list = term_list { t::list }
*)

main:
| o = objective cl = ineqs EOF { (o, cl, []) }
| o = objective cl = ineqs point = basic_point EOF { (o, cl, point) }


objective:
| MAXIMIZE l = linear_form SEMICOLON {
  List.rev_map (fun (j, v) -> (j, Neg, v) ) l
}
| MINIMIZE l = linear_form SEMICOLON{
  List.rev_map (fun (j, v) -> (j, Pos, v) ) l
}

ineqs:
| l = nonempty_list(ineq) {l}

ineq:
| l1 = linear_form LEQ l2 = linear_form SEMICOLON {
  let lpos = List.rev_map (fun (j, v) -> (j, Pos, v) ) l2 in
  let lneg = List.rev_map (fun (j,v) -> (j, Neg, v) ) l1 in
  List.rev_append lpos lneg
}
| l1 = linear_form GEQ l2 = linear_form SEMICOLON {
  let lpos = List.rev_map (fun (j, v) -> (j, Pos, v) ) l1 in
  let lneg = List.rev_map (fun (j,v) -> (j, Neg, v) ) l2 in
  List.rev_append lpos lneg
}
(*| linear_form EQ linear_form SEMICOLON {}*)


basic_point:
| BASIC_POINT EQ l = separated_nonempty_list(COMMA, num_expr) SEMICOLON {l}

(*
ll:
|l = linear_form  EOF {l}
const_eof:
| uh=const EOF { uh}
consts_eof:
| uh=consts EOF { uh}



*)
(*
| c = const { c::[] }
| c = const cl=consts { List.append c  cl}
*)
