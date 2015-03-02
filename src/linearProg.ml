
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

(* depends on Group*)


type sign_t = Pos | Neg

type col_index_t  = Affine | Var of int
type row_index_t  = Objective | Ineq of int


(** Tropical linear program

  of the form
    min  max ( c_1 + x_1, ..., c_n + x_n ) 
    s.t. for i=1...m 
         max ( a^+_i1 + x_1 , ..., a^+_in + x_n, b^+_i) >= max ( a^-_i1 + x_1 , ..., a^-_in + x_n, b^-_i)
*)
module type T =
sig
  type t 

  (** The coefficients of the LP belongs to the group {b Entries} *)
  module Entries : Group.Ordered

  (** number n of variables *)
  val dim : t -> int

  (** number m of inequalities *)
  val nb_ineq : t -> int
 
 

 
  (** {b init n objective ineqs}
      create a new tropical linear program with {b n} variables, where:
      - {b objective} is the list of coefficients for the objective function
      - {b ineqs} is the list of inequalities, each inequality being a list of coefficient
      
      Each coefficient is of the form (col_index, sign, entry) where:
        - col_index is either Affine of Var j; where 0 <= j <= n-1
        - sign is either {e Pos} or {e Neg}
        - entry is the coefficient for variable col_index in the row. 

      In inequality rows, coefficients with {e Pos} sign are on the left of >= and coefficient with {e Neg} sign are of the right of >=;
      eg (Var 0, Pos, 3); (Affine, Pos, 4); (Var 1, Neg, 5)  encodes  max( x0 + 3, 4 ) >= x1 + 5

      - a row cannot be defined twice
      - an coefficient (i,j) can be defined more than once; 
        only the greatest coefficient is kept; if the greatest coefficient appears twice with opposite signs, the Pos sign is kept
      - in the Objective row, all signs must be Pos
      - the positive row can have an Affine coefficient, it will have no effect
  *)  
  val init : 
    ?var_names:(int -> string)
    -> int (* nb_var*)
    -> (col_index_t * sign_t * Entries.t) list (* objective *)
    -> ((col_index_t * sign_t * Entries.t) list) list (* ineqs *)
    -> t





  val get_row : t -> row_index_t -> (col_index_t * sign_t * Entries.t) list
  val get_col : t -> col_index_t -> (row_index_t * sign_t * Entries.t) list
 

   (** {b compute_slack_args lp i point}
       computes the set J = argmax_j |w_ij| + x_j 
       where w_i = (A_i b_i) is the row indexed by {b i} of {b lp} and x = {b point}

       return the list  (j, sign w_ij, |w_ij|) for j in J 
   *)
  val compute_slack_args : t -> row_index_t -> Entries.t array ->  (col_index_t * sign_t *  Entries.t) list 


    (** {b compute_entry_plus_var entry j x }
        return entry + x_j if col_index = Var j
        return entry otherwise
    *)
  val compute_entry_plus_var : Entries.t-> col_index_t-> Entries.t array -> Entries.t 



  val is_point_feasible : t -> Entries.t array -> bool


  val print : t -> out_channel option-> unit
  val pretty_print : t -> out_channel option ->  unit

  val get_var_names : t -> (int -> string)

 
    
end

(**
Create module for tropical linear programs with coefficients in the ordered group {b G}
*)
module Make (G: Group.Ordered) : (T with module Entries = G ) =
struct
  module Entries = G
    
 





  type t ={
    (* vars *)
    vars : ((row_index_t * sign_t * Entries.t) list) array;
    affine_col :  (row_index_t * sign_t * Entries.t) list;
 
    (* ineq *)
    objective : (col_index_t * sign_t * Entries.t) list ; 
    ineqs : ( (col_index_t * sign_t * Entries.t) list) array;

    var_names : int -> string;

}
 
  let get_var_names t = t.var_names

      
  let dim lp = Array.length lp.vars
  let nb_ineq lp = Array.length lp.ineqs

   
  let get_row lp row_index =
    match row_index with
      | Objective -> lp.objective
      | Ineq i -> Array.get lp.ineqs i

  let get_col lp col_index =
    match col_index with
      | Affine -> lp.affine_col
      | Var j -> Array.get lp.vars j

  let compute_entry_plus_var entry col_index point =
    match col_index with 
      | Affine -> entry
      | Var j -> let x_j = Array.get point j in
                 Entries.add entry x_j

      
  let compute_slack_args lp row_index point =
    let row = get_row lp row_index in
    let compute_slack_args_aux old_arg (col_index, sign, entry) =
      let slack = compute_entry_plus_var entry col_index point in
      match old_arg with
        | [] -> (col_index, sign, entry)::[]
        | (old_col_index, old_sign, old_entry)::_ -> 
            let old_slack =  compute_entry_plus_var old_entry old_col_index point in
            begin
              match (Entries.compare slack old_slack) with
                | 0  -> (col_index, sign, entry)::old_arg
                | 1  -> (col_index, sign, entry)::[]
                | -1 ->  old_arg
                | _  -> assert false
            end
    in
    List.fold_left compute_slack_args_aux [] row 




  let is_point_feasible lp point =
    if dim lp <> (Array.length point) then
      invalid_arg ("Error, linearProgram.is_point_feasible: dimesion mismatch between point and lp");
    
    let rec check_ineq i =
      if i >= (nb_ineq lp) then true
      else
        let arg = compute_slack_args lp (Ineq i) point in
        let (pos, neg) =
          List.fold_left ( fun (pos, neg) (col_index, sign, entry) ->
            match sign with
              | Pos -> (true, neg)
              | Neg -> (pos, true)
          ) (false, false) arg
        in
        match (pos, neg) with
          | (false, false) -> assert false
          | ( true, _) -> check_ineq (i+1)
          | (false, true) -> false
    in
    check_ineq 0
    
    

      



  let entry_to_string row_index col_index entry =
    let row_s = match row_index with
      | Objective -> "objective"
      | Ineq i -> "ineq "^(string_of_int i)
    in
    let col_s = match col_index with
      | Affine -> "affine"
      | Var j -> "var "^(string_of_int j)
    in
    row_s^", "^col_s^": "^(Entries.to_string entry) 


  let default_var_names j =
    "x"^(string_of_int j)



  let pretty_print_entry ?var_names:(var_names=default_var_names)  col_index  entry =
    let compare_entry_with_zero = Entries.compare entry Entries.zero in
    let op_s = ""^(Entries.operation_string)^"" in
    match (col_index,  compare_entry_with_zero ) with
      | (Var j, 0) ->  var_names j  
      | (Var j, _) ->  (Entries.to_string entry)^op_s^ (var_names j)
      | (Affine, _) -> Entries.to_string entry

(* just print nothing if there is no outchannel given *)
  let myprintf outchannel =
    match outchannel with
      | None -> Printf.ifprintf stdout
      | Some c -> Printf.fprintf c

  let myflush outchannel =
      match outchannel with
        | None -> ()
        | Some c -> flush c


  let pretty_print_scalar_product ?var_names:(var_names=default_var_names) row outchannel =
      match row with
        | [] -> myprintf outchannel "-oo";
        | (col_index, entry)::[] -> myprintf outchannel "%s" (pretty_print_entry ~var_names:var_names  col_index entry);
        | (col_index,entry)::tail ->
            myprintf outchannel "max( %s" (pretty_print_entry ~var_names:var_names col_index entry);
            List.iter (fun (col_index, entry) ->  myprintf outchannel ", %s" (pretty_print_entry ~var_names:var_names col_index entry) ) tail;
            myprintf outchannel " )"

  let pretty_print_ineq t ineq_index outchannel = 
    let row = Array.get t.ineqs ineq_index in
    (* separate entries with positive and negative sign *)
    let process_row (row_pos, row_neg) (col_index, sign, entry) =
      match sign with
        | Pos ->  ((col_index, entry) :: row_pos , row_neg)
        | Neg -> (row_pos ,  (col_index, entry) :: row_neg) 
    in
    let (row_pos, row_neg) = List.fold_left process_row ([], []) row in
    pretty_print_scalar_product ~var_names:t.var_names row_pos outchannel;
    myprintf outchannel " >= ";
    pretty_print_scalar_product ~var_names:t.var_names row_neg outchannel
    

  let pretty_print_objective t outchannel =
    let row = t.objective in
    let process_row (row_pos, row_neg) (col_index, sign, entry) =
      match sign with
        | Pos ->  ((col_index, entry) :: row_pos , row_neg)
        | Neg -> (row_pos ,  (col_index, entry) :: row_neg) 
    in
    let (row_pos, row_neg) = List.fold_left process_row ([], []) row in
    match (row_pos, row_neg) with
      | ([],[]) -> myprintf outchannel "no objective"
      | (_, []) -> 
          begin 
            myprintf outchannel "minimize ";
            pretty_print_scalar_product ~var_names:t.var_names row_pos outchannel
          end
      | ([], _) -> 
          begin
            myprintf outchannel "maximize ";
            pretty_print_scalar_product ~var_names:t.var_names row_neg outchannel
          end
      | _ -> assert false (*sould not have positive and negative coeffs in objective function *)
      

  let pretty_print t outchannel=  
    myprintf outchannel "dim = %i\n" (dim t);
    myprintf outchannel "nb ineq = %i\n" (nb_ineq t);
    
 
    pretty_print_objective t outchannel;
    myprintf outchannel "\nsubject to:\n";
    for i=0 to (nb_ineq t)-1 do
      pretty_print_ineq t i outchannel;
      myprintf outchannel "\n";
      myflush outchannel
    done
      


  let signed_entry_to_string sign entry = 
    match sign with 
      |Pos -> "tpos "^(Entries.to_string entry)
      |Neg -> "tneg "^(Entries.to_string entry)

  let print_row t outchannel row_index = 
    let row = match row_index with
      | Objective -> t.objective
      | Ineq i -> Array.get t.ineqs i in
    List.iter (fun (col_index, sign, entry) -> 
      match col_index with
        | Affine -> myprintf outchannel "affine, %s" (signed_entry_to_string sign entry)
        | Var j -> myprintf outchannel "%i, %s \t" j (signed_entry_to_string sign entry) )
      row



  let print t outchannel =
    myprintf outchannel "nb var \t= %i\n" (dim t);
    myprintf outchannel "nb halfspace \t= %i\n" (nb_ineq t);
    myprintf outchannel "\nhalfspaces:\n";
    for i = 0 to ((nb_ineq t) -1) do
      myprintf outchannel "%i: \t" i;
      print_row t outchannel (Ineq i);
      myprintf outchannel "\n";
    done;  
    myprintf outchannel "\nobjective:\n";
    print_row t outchannel Objective;
    myprintf outchannel "\n";
    myflush outchannel
    



  let init ?var_names:(var_names=default_var_names) nb_var obj ineqs   =

    (* building the matrix *)

    (* build columns from input *)
    let vars = Array.make nb_var [] in
    let affine_col = ref [] in


    let process_input_row row_index w_i =   
      (* check that row has not been defined previously *)

              
      (* check whether the coeff (row_index, col_index) has been defined previously
         if it has been, keep the relevent coefficient *)
      let process_input_entry (col_index, sign, entry) = 
        begin
          match (row_index, sign) with 
 (*           | (Objective, Neg) -> invalid_arg ("Error while adding objective row: input contains a tropically negative entry "^(pretty_print_entry ~var_names:var_names col_index  entry)) *)
            | _ -> ()
        end;
        let old_col = 
          match col_index with
            | Affine -> !affine_col
            | Var j ->
                try Array.get vars j 
                with  | Invalid_argument (s) -> invalid_arg ("Error while adding rows: "^(string_of_int j)^" is not a valid variable index")
        in
        let new_col =
          match old_col with
            | (old_row_index, old_sign, old_entry):: col_tail when old_row_index = row_index ->
                begin
                  let row_s = match row_index with
                    | Objective -> "objective"
                    | Ineq i -> "ineq "^(string_of_int i)
                  in
                  let col_s = match col_index with
                    | Affine -> "affine"
                    | Var j -> "var "^(string_of_int j)
                  in
                  Printf.eprintf "Warning: entry (%s, %s) is defined twice : " row_s col_s
             
                end;
                Printf.eprintf "(%s) and (%s) \n" 
                  (signed_entry_to_string sign entry) 
                  (signed_entry_to_string old_sign old_entry);
                flush stderr;
                let new_col_head =
                  if sign = old_sign then
                    (row_index, sign, Entries.max entry old_entry )
                  else 
                    match (Entries.compare entry old_entry) with
                      | 0  -> (row_index, Pos, entry)
                      | 1  -> (row_index, sign, entry)
                      | -1 -> (row_index, old_sign, old_entry)
                      | _  -> assert false
                in
                new_col_head :: col_tail
            | col_tail -> (row_index, sign, entry) :: col_tail
                
        in
        match col_index with
          | Affine -> affine_col := new_col
          | Var j -> Array.set vars j new_col
      in
      List.iter process_input_entry w_i;
    
      
    in
    List.iteri (fun i w_i ->  process_input_row (Ineq i) w_i) ineqs;
    process_input_row Objective obj;
  
    (* build rows from columns *)
    let nb_ineq = List.length ineqs in
    let ineqs = Array.make nb_ineq [] in
    let objective_row = ref [] in
    let process_col col_index w_j =
      let add_entry_to_row (row_index, sign, entry) = 
        let old_row =
          match row_index with
            | Objective -> !objective_row
            | Ineq i -> Array.get ineqs i 
        in
        let new_row = (col_index, sign, entry) :: old_row in
        match row_index with
          | Objective -> objective_row := new_row
          | Ineq i -> Array.set ineqs i new_row
      in
      List.iter add_entry_to_row w_j
    in
    process_col Affine !affine_col;
    Array.iteri (fun j w_j -> process_col (Var j) w_j) vars;
   

 { vars = vars; ineqs = ineqs; affine_col = !affine_col; objective = !objective_row; var_names = var_names }

          




end





