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

(* depends on LinearProg, TangentDigraph *)
open LinearProg
open TangentDigraph


(** A simplet is a tropical linear program and a basic point of this program

Implements the pivoting operation and the computation of reduced costs described in Chapter7 of
      {e Tropical aspects of linear programming}, Pascal Benchimol, PhD Thesis, 2014

*)


module type T =
sig
  module Entries : Group.Ordered
  module LP: LinearProg.T with module Entries = Entries
  module TangentDigraph : TangentDigraph.T with module Entries = Entries
  type t 
 
 
    
  (** {b init lp x} create a simplet for {b lp} at point {b x}
      - {b lp} must be non-degenerate, ie the matrix {v
       |c .|  
       |A b| 
       v} must not have a tropically singular submatrix with non null permanent

      - {b x} must be a basic point of {b lp}, ie be feasible and saturate exactly (dim {b lp}) inequalities
    *)
  val init : LP.t 
    ->  Entries.t array  (*basic point*)
    -> t



  (** {b pivot simplet ineq_index }
      pivot from the current basic point along the edge defined by I \ {ineq_index},
      where I is the set of inequalities saturated by the current basic point of {b simplet} and {b ineq_index } belongs to I
      
  *)
  val pivot : t -> int -> unit
    

  (** choose the first inequality with negative reduced cost,
        return None if none exists *)
  val bland_rule : t -> ineq_index_t option
    
  (** {b solve simplet pivoting_rule }
      repeatedly call {b pivoting_rule} and {b pivot} until {b pivoting_rule} returns None *) 
  val solve : t -> ( t -> ineq_index_t option) -> out_channel option -> unit

  (** {b basis_contains simplet ineq_index}
      returns true if the current basic point of {b simplet} saturates the inequality {b ineq_index}   
  *)
  val basis_contains : t -> int -> bool

  (** {b red_cost simplet ineq_index}
      returns the reduced cost for the inequality  {b ineq_index}  at the current basis of {b simplet}
  *)
  val red_cost : t -> int ->  (sign_t * Entries.t) option

  val lp : t -> LP.t
  val tangent_digraph : t -> TangentDigraph.t
  val basic_point : t -> Entries.t array
  val print : t -> out_channel option -> unit



end

module Make (LP: LinearProg.T)
: (T with module Entries = LP.Entries 
  with module LP = LP
) =
struct
  module Entries = LP.Entries
  module LP = LP (*LinearProg.Make(Group)  *)
  module TangentDigraph = TangentDigraph.Make(LP)


  (* during pivoting, possible status of inequalities *)
  type ineq_status_t = BreakHyp  | Basis | EntHyp | Inactive


  type t ={
  
    lp : LP.t;
    point : Entries.t array;

    tangent_digraph : TangentDigraph.t;

    (* graph between Ineq i and argmax_j (|W^+_ij| + x_j) *)
    arg_slacks :  ((var_index_t *  Entries.t) list) array;
    var_in_arg_slack : (ineq_index_t list) array;
    mutable affine_var_in_arg_slack : ineq_index_t list;

    (* for pivoting *)
    mutable direction : var_index_t list;
    (* argmax_{ j in direction}  |W_ij| + x_j *)
    arg_lambdas : ((var_index_t * sign_t*  Entries.t) list) array;
    ineq_status : ineq_status_t array;

    (* for reduced costs *)
    max_permutation :  ((ineq_index_t * sign_t * Entries.t) option) array;
    reduced_costs : ((sign_t * Entries.t) option) array; 
    dual_slacks :  ((sign_t * Entries.t) option) array;
    var_seen : bool array; (* during dijkstra: true if var already in longest path tree *)

}
      

  let basic_point simplet = simplet.point
  let lp simplet = simplet.lp

  let tangent_digraph simplet = simplet.tangent_digraph

  let red_cost simplet i = 
    Array.get simplet.reduced_costs i



  let print_reduced_costs simplet outchannel = 
    Printf.fprintf outchannel "reduced costs:\n";
    Array.iteri( fun i red_cost ->
      let red_cost_s =
        match red_cost with
          | None -> "null"
          | Some (Pos, entry) -> Format.sprintf "Pos %s" (Entries.to_string entry)
          | Some (Neg, entry) -> Format.sprintf "Neg %s" (Entries.to_string entry)
      in
      Printf.fprintf outchannel "y%i: %s\n" i red_cost_s;

    ) simplet.reduced_costs;
    flush outchannel

  let print_point simplet outchannel =
    
    let var_names = LP.get_var_names (simplet.lp) in
    Array.iteri( fun j x ->
      Printf.fprintf outchannel "%s: %s\n" (var_names j) (Entries.to_string x);

    ) simplet.point;
    flush outchannel

  let print_basis simplet outchannel =
    let nb_ineq = LP.nb_ineq simplet.lp in
    Printf.fprintf outchannel "basis:\n";
    for i=0 to (nb_ineq - 1) do
      let is_hyp_node = TangentDigraph.is_hyp_node simplet.tangent_digraph i in
      if (is_hyp_node) then
        Printf.fprintf outchannel "%i, " i
    done;
    Printf.fprintf outchannel "\n";
    flush outchannel


  let print simplet outchannel = 
    match outchannel with 
      | None -> ()
      | Some c ->
          begin
            print_basis simplet c;
            Printf.fprintf c "point:\n";
            print_point simplet c;
            print_reduced_costs simplet c;
            flush c
          end

  let  basis_contains simplet ineq_index =
    TangentDigraph.is_hyp_node simplet.tangent_digraph ineq_index



  let bland_rule simplet =
    let nb_ineq = LP.nb_ineq simplet.lp in
    let rec find_first_negative_reduced_cost ineq_index =
      if (ineq_index >= nb_ineq) then None
      else
        let is_hyp_node = TangentDigraph.is_hyp_node simplet.tangent_digraph ineq_index in
        if not (is_hyp_node) then
          find_first_negative_reduced_cost (ineq_index + 1)
        else
          let red_cost = Array.get simplet.reduced_costs ineq_index in
          match red_cost with
            | None | Some (Pos,_) -> find_first_negative_reduced_cost (ineq_index + 1)
            | Some (Neg, _ ) -> Some ineq_index
    in
    find_first_negative_reduced_cost 0
        


  (* compute graph between var j and argmax_j (W^+_ij + x_j)  and store it in simplet *)
  let compute_arg_slacks_pos simplet =
    let nb_ineq = LP.nb_ineq simplet.lp in
    let dim = LP.dim simplet.lp in
    Array.fill simplet.arg_slacks 0 nb_ineq [];
    Array.fill simplet.var_in_arg_slack 0 dim [];
    simplet.affine_var_in_arg_slack <- [];

    for i = 0 to (nb_ineq -1) do
      let arg_slack = LP.compute_slack_args simplet.lp (Ineq i) simplet.point in
      let proceed_var (var_index,sign,entry) = 
        match sign with
          | Neg -> ()
          | Pos -> let old_arg_slack_pos = Array.get simplet.arg_slacks i in
                   Array.set simplet.arg_slacks i ((var_index,entry)::old_arg_slack_pos);
                   match var_index with
                     | Affine -> simplet.affine_var_in_arg_slack <- i :: simplet.affine_var_in_arg_slack
                     | Var j -> let old_list = Array.get simplet.var_in_arg_slack j in
                                Array.set simplet.var_in_arg_slack j (i :: old_list)
      in
      List.iter proceed_var arg_slack
    done
 





  (* during pivoting, compute bound on length of the current ordinary segment given by inequality ineq_index   *)
  let bound_on_length_by_ineq simplet ineq_index =
    let arg_slack = Array.get simplet.arg_slacks ineq_index in
    let arg_lambda = Array.get simplet.arg_lambdas ineq_index in
    let ineq_status = Array.get simplet.ineq_status ineq_index in
    match (arg_slack, arg_lambda) with
      | ([], _) -> invalid_arg ("Error, simplet.compare_bounds_on_length_by_ineq: slack of ineq_1 is +oo")
      | (_,  []) ->  None
      |  ((var_index_slack, entry_slack)::_ ,(var_index_lambda, sign_lambda, entry_lambda)::_ ) -> 
              match (ineq_status, sign_lambda) with
                | (Basis,_) | (Inactive,_) -> None
                | (EntHyp, Pos) -> None
                | (BreakHyp,_) | (EntHyp, Neg) ->
                    let slack = LP.compute_entry_plus_var entry_slack var_index_slack simplet.point in
                    let max_J = LP.compute_entry_plus_var entry_lambda var_index_lambda simplet.point in
                    let bound = Entries.substract slack max_J in
                    Some bound


  (* traverse_breakHyp simplet breakHyp_index

     let [x, x'] u [x', x''] be two consecutive ordinary segments of a tropical edge
     before calling:
     -simplet.tangent_digraph is the tangent digraph in ]x, x'[
     -simplet.direction is direction of [x,x']
     -simplet.ineq_status is ineq status at x
     -simplet.arg_lambdas is arg_lambdas at x

     the breakpoint x' is defined by the inequality breakHyp_index
     
     after calling:
     -simplet.tangent_digraph is the tangent digraph in ]x', x''[
     -simplet.direction is direction of [x',x'']
     -simplet.ineq_status is ineq status at x'
     -simplet.arg_lambdas is arg_lambdas at x'

 *)
  let traverse_breakHyp simplet breakHyp_index =
    let arg_lambda = Array.get simplet.arg_lambdas breakHyp_index in
    let (new_arc_var_index, new_arc_sign, new_arc_entry ) =
      match arg_lambda with
        | [] -> assert false
        |  _ :: _ :: _ -> invalid_arg ("Error, simplet.traverse_ordinary_segment: length = lambda_i where arg_lambda_i is not a singleton.")
        |  arc :: [] -> arc
    in
    let ineq_node = TangentDigraph.get_ineq_node simplet.tangent_digraph breakHyp_index in
    (*
    let var_node_s = match new_arc_var_index with
      | Affine -> "Affine"
      | Var j -> ("Var "^(string_of_int j))
    in
    
    begin
      match new_arc_sign with
        | Pos -> Format.printf "VarNode %s to IneqNode %i\n" var_node_s breakHyp_index
        | Neg -> Format.printf "IneqNode %i to VarNode %s\n" breakHyp_index var_node_s
    end;
    *)

    (* get old arc incident to ineq node with correct orientation *)
    let (old_arcs,_) = 
      List.partition (fun (_,sign,_) -> sign = new_arc_sign)
        ineq_node
    in
    let old_arc_var_index = 
      match old_arcs with
        | [] | _ :: _ :: _ -> invalid_arg ("Error, simplet.traverse_breakHyp: breakHyp node is not of degree 2")
        | (var_index, _,_) :: [] -> var_index
    in

    (* remove old arc *)
    TangentDigraph.remove_arc simplet.tangent_digraph old_arc_var_index breakHyp_index;


    (* update break hyp and direction*)
    let update_break_hyp_and_direction new_direction_vars node_index =
      match node_index with
        | IneqNode i -> 
           (* Format.printf "Ineq %i: "i; *)
            begin
              match (Array.get simplet.ineq_status i) with
                | BreakHyp -> Array.set simplet.ineq_status i Basis
                | _ -> assert false
(*
                | Basis -> Format.printf "basis";
                    assert false
                | Inactive -> Format.printf "inactive";
                           assert false
                | EntHyp -> Format.printf "ent";
assert false *)
            end;
            new_direction_vars
        | VarNode var_index -> var_index::new_direction_vars
    in
    let new_direction_vars = TangentDigraph.dfs_fold_acyclic_graph simplet.tangent_digraph update_break_hyp_and_direction []  (IneqNode breakHyp_index) in
    
    (*
    Format.printf "new direction vars: \n";
    List.iter (fun var_index -> match var_index with
      | Affine -> Format.printf "affine"
      | Var j -> Format.printf "Var %i, " j
    ) new_direction_vars;
    Format.printf "\n\n";

    Format.printf "break hyp: \n";
    Array.iteri (fun i status -> 
      match status with
        | BreakHyp -> Format.printf "%i, "i
        | _ -> ()
    ) simplet.ineq_status;
    Format.printf "\n\n";
    *)


    simplet.direction <- List.append new_direction_vars simplet.direction;


    (* update ent hyp *)
    List.iter (fun var_index -> 
      let list = match var_index with
        | Affine -> simplet.affine_var_in_arg_slack
        | Var j -> Array.get simplet.var_in_arg_slack j
      in
      List.iter (fun ineq_index -> 
        match (Array.get simplet.ineq_status ineq_index ) with
          | EntHyp -> Array.set simplet.ineq_status ineq_index Inactive
          | Inactive -> ()
          | BreakHyp -> assert false
          | Basis -> ()
      ) list
    ) new_direction_vars;

    (*
    Format.printf "ent hyp: \n";
    Array.iteri (fun i status -> 
      match status with
        | EntHyp -> Format.printf "%i, "i
        | _ -> ()
    ) simplet.ineq_status;
    Format.printf "\n\n";
    *)

     (* add new arc *)
    TangentDigraph.add_arc simplet.tangent_digraph new_arc_var_index breakHyp_index new_arc_sign new_arc_entry;

    (* update arg lambda *)
    List.iter ( fun var_index -> 
      let col_j = LP.get_col simplet.lp var_index in
      let compute_args  (row_index, sign, entry) =
        match row_index with
          | Objective -> ()
          | Ineq ineq_index -> 
              let old_arg = Array.get simplet.arg_lambdas ineq_index in
              let value = LP.compute_entry_plus_var entry var_index simplet.point in
              let new_arg =
                match old_arg with
                  | [] -> (var_index, sign, entry)::[]
                  | (old_var_index, old_sign, old_entry)::_ -> 
                      let old_value =  LP.compute_entry_plus_var old_entry old_var_index simplet.point in
                      begin
                        match (Entries.compare value old_value) with
                          | 0  -> (var_index, sign, entry)::old_arg
                          | 1  -> (var_index, sign, entry)::[]
                          | -1 ->  old_arg
                          | _  -> assert false
                      end
              in    
              Array.set simplet.arg_lambdas ineq_index new_arg
      in   
      List.iter compute_args col_j 
    ) new_direction_vars;
    
    (*
    Format.printf "lambdas: \n";
    for i = 0 to ((LP.nb_ineq simplet.lp)- 1) do
      Format.printf "Ineq %i: " i;
      let arg = Array.get simplet.arg_lambdas i in
      let lambda = bound_on_length_by_ineq simplet i in
      begin
        match lambda with
          | None -> Format.printf "+oo, "
          | Some l -> Format.printf "%s, " (Entries.to_string l)
      end;
            
      begin
        match arg with 
          | [] -> assert (lambda = None)
          | (var_index, sign, entry)::_ -> 
              begin
                List.iter (fun (var_index, sign, _) -> 
                  begin
                    match var_index with
                      | Affine -> Format.printf "Affine "
                      | Var j -> Format.printf "Var %i " j
                  end;
                  begin 
                    match sign with
                      | Pos -> Format.printf "Pos,"
                      | Neg -> Format.printf "Neg,"
                  end                
                ) arg
              end;
      end;
      Format.printf "\n"
        
    done;
    *)
  
    (* arg slack change for breakHyp_index; no need to update
       does not change  for other BreakHyp 
       does not change for EntHyp *)


    ()
      
    
  (*
    traverse_ordinary_segment simplet

    let [x, x'] be an ordinary segment of a tropical edge
    before calling:
    -simplet.point is x
    -simplet.tangent_digraph is the tangent digraph in ]x, x'[
    -simplet.direction is direction of [x,x']
    -simplet.ineq_status is ineq status at x
    -simplet.arg_lambdas is arg_lambdas at x

    after calling:
    if x' is a basic point then:
    -simplet.point is x'
    -simplet.tangent_digraph is the tangent digraph in ]x, x'[
    return i_ent the inequality defining the basic point x'

    if x' is a breakpoint, and the next ordinary segment is [x',x''] then
    -simplet.point is x'
    -simplet.tangent_digraph is the tangent digraph in ]x', x''[
    -simplet.direction is direction of [x',x'']
    -simplet.ineq_status is ineq status at x' for [x',x'']
    -simplet.arg_lambdas is arg_lambdas at x' for [x',x'']

*)
  let traverse_ordinary_segment simplet=
    let nb_ineq = LP.nb_ineq simplet.lp in
    let dim = LP.dim simplet.lp in

    (* compute length of the segment *)
    let rec compute_length length arg_length ineq_index =
      if ineq_index >= nb_ineq then (length, arg_length)
      else
        let bound_i =  bound_on_length_by_ineq simplet ineq_index in
        match (length, bound_i) with
          | (_, None) -> compute_length length arg_length (ineq_index+1)
          | (None, Some _) -> compute_length bound_i (ineq_index::[]) (ineq_index+1)
          | (Some l, Some b) -> 
              match (Entries.compare l b) with
                | 1 -> compute_length bound_i (ineq_index::[]) (ineq_index+1)
                | 0 -> compute_length length (ineq_index::arg_length) (ineq_index+1)
                | -1 -> compute_length length arg_length (ineq_index+1)
                | _ -> assert false
    in
    let (length_option, arg_length) = compute_length None [] 0 in
    let (length, length_ineq_index ) =
      match (length_option, arg_length) with
        | (None, []) -> invalid_arg ("Error, simplet.traverse_ordinary_segment: unbounded ordinary segment")
        | (Some _ , []) | (None, _::_) -> assert false
        | (Some _, _ :: _ :: _) ->  invalid_arg ("Error, simplet.traverse_ordinary_segment: length defined by multiple inequalities")
        | (Some length, length_ineq_index :: []) -> (length, length_ineq_index)
    in
    
    (* Format.printf "\nlength :%s\n" (Entries.to_string length); *)

    (*update point value*)
    let rec direction_contains_affine_var direction=
      match direction with
        | Affine::tail -> true
        | [] -> false
        | _ ::tail -> direction_contains_affine_var tail
    in
    
    if( direction_contains_affine_var simplet.direction ) then
      for j = 0 to (dim -1) do
        let old_x_j = Array.get simplet.point j in
        let new_x_j = Entries.substract old_x_j length in
        Array.set simplet.point j new_x_j
      done;
    
    List.iter (fun var_index ->
      match var_index with
        | Affine -> ()
        | Var j -> let old_x_j = Array.get simplet.point j in
                   let new_x_j = Entries.add old_x_j length in
                   Array.set simplet.point j new_x_j
    ) simplet.direction;

      (* Array.iteri (fun j entry -> Format.printf "x%i = %s\n" j (Entries.to_string entry)) simplet.point; *)


      let ineq_status = Array.get simplet.ineq_status length_ineq_index in
      
      (* Format.printf "length ineq index %i\n" length_ineq_index; *)
      
      match ineq_status with 
        | Inactive | Basis -> assert false
        | BreakHyp -> traverse_breakHyp simplet length_ineq_index;
            None
        | EntHyp -> Some length_ineq_index




      

  (* compute_max_permutation simplet
     at basic point x, with basis I,
     
     compute the maximizing permutation sigma : [n] -> I in
     the tropical permanent of A_I
  *)
  let compute_max_permutation simplet =
    let dim = LP.dim simplet.lp in
    Array.fill simplet.max_permutation 0 dim None;

    (* perform a depth first seach in the tangent digraph starting from variable node Affine
       match visisted ineq_node to its only varnode neighboor not yet in the permutation
    *)
    let compute_sigma  node =
      match node with
        | VarNode _ ->  ()
        | IneqNode i ->
            let ineq_node = TangentDigraph.get_ineq_node simplet.tangent_digraph i in
            let (j, sign, entry) = 
              match ineq_node with
                | [] | _ :: [] |  _ :: _ :: _ :: _ -> assert false
                | (Affine, _, _) :: (Affine, _, _) :: [] -> assert false
                | (Var j, sign, entry) :: (Affine, _, _) :: [] -> (j, sign, entry)
                | (Affine, _, _) :: (Var j, sign, entry) :: [] -> (j, sign, entry)
                | (Var j, sign_j, entry_j) :: (Var l, sign_l, entry_l):: [] ->
                    let sigma_j_defined  = 
                      match (Array.get simplet.max_permutation j) with
                        | None -> false
                        | Some _ -> true
                    in
                    let sigma_l_defined =
                      match (Array.get simplet.max_permutation l) with
                        | None -> false
                        | Some _ -> true
                    in
                    match (sigma_j_defined, sigma_l_defined) with
                      | (true, true) | (false, false) -> assert false
                      | (true, false) -> (l, sign_l, entry_l)
                      | (false, true) -> (j, sign_j, entry_j)
            in
            Array.set simplet.max_permutation j (Some (i, sign, entry))
    in
    
    TangentDigraph.dfs_fold_acyclic_graph simplet.tangent_digraph  (fun _ node -> compute_sigma node)  () (VarNode Affine); 

    Array.iteri (fun j sigma_j -> 
      match sigma_j with
        | None ->  invalid_arg(Format.sprintf "Error while computing maximizing permutation; sigma(%i) not defined "  j)
        | _ -> ()
    ) simplet.max_permutation;
    
(*
    Format.printf "max permutation :\n";
    Array.iteri ( fun j sigma_j ->
      match sigma_j with
        | None -> assert false
        | Some (i,_,_) -> Format.printf "sigma( %i ) = %i \n" j i
    ) simplet.max_permutation;
*)  
    ()



  
  let compute_reduced_costs simplet =
    let nb_ineq = LP.nb_ineq simplet.lp in
    let dim = LP.dim simplet.lp in
    
    compute_max_permutation simplet;

    (* contains distance from Node Objective to IneqNodes *)
    Array.fill simplet.reduced_costs 0 nb_ineq None;

    (* contains distance from Node Objective to VarNodes*)
    Array.fill simplet.dual_slacks 0 dim None;

    (* for Dijkstra: become true when VarNode is in the tree of longest paths *)
    Array.fill simplet.var_seen 0 dim false;
    

    (* mu = max(0, c_1+x_1, ..., c_n+x_n) *)
    let compute_mu mu (var_index, sign, entry) =
      match var_index with 
        | Affine -> mu
        | Var j -> 
            let current_val = LP.compute_entry_plus_var entry var_index simplet.point in
            Entries.max mu current_val
    in
    let mu = List.fold_left compute_mu Entries.zero (LP.get_row simplet.lp Objective)
    in
    let neg_mu = Entries.neg mu in
     
        
      


    (* first step of Dijkstra's algorithm for longest paths from Node Objective:
       init distances from Node Objective to its neighboors *)
    List.iter (
      fun (var_index, sign, entry) ->
        match var_index with
          | Affine-> ()
          | Var j ->
              let x_j = Array.get simplet.point j in
              let scaled_entry = Entries.sum (neg_mu::x_j::entry::[])  in
              (*
              let s = match sign with | Pos -> "pos" | Neg -> "neg" in
             Format.printf " arc Objective to var %i with weigth %s %s\n" j s (Entries.to_string scaled_entry);  *)
(* Format.printf " arc Objective to var %i with weigth %s %s\n" j s (Entries.to_string entry);  *)
              Array.set simplet.dual_slacks j (Some (sign, scaled_entry))            
    ) (LP.get_row simplet.lp Objective);


    (* find the VarNode with the longest distance not yet in the tree of longest paths *)
    let rec find_max max_j current_j  =
     (* Format.printf "find max at var %i\n" current_j; *)
        if (current_j >= dim) then max_j
        else if (Array.get simplet.var_seen current_j) then find_max max_j (current_j +1)
        else
          let current_val = Array.get simplet.dual_slacks current_j in
          let max_val = match max_j with
            | None -> None 
            | Some j -> Array.get simplet.dual_slacks j
          in
          let new_max_j =
            match (max_val, current_val) with
              | (_, None) -> max_j
              | (None, Some (cur_s, cur_e)) ->  (* Format.printf "var node %i has value %s\n" current_j (Entries.to_string cur_e); *)
                  Some current_j
              | (Some (max_sign, max_entry), Some (current_sign, current_entry)) ->
                  begin
                  (*  Format.printf "var node %i has value %s\n" current_j (Entries.to_string current_entry); *)
                    match (Entries.compare current_entry max_entry) with
                      | 1 -> Some current_j
                      | 0 | -1 -> max_j
                      | _ -> assert false
                  end
          in
         (* begin
          match new_max_j with
            | None -> Format.printf "new max is none\n"
            | Some j -> Format.printf "new max is var %i\n" j
          end;*)
          find_max new_max_j (current_j+1)
      in
    
    (* Dijkstra: 
       consider only VarNodes;
       When considering a VarNode j:
       -mark VarNode j as seen (ie in the tree of longest paths)
       -go to IneqNode sigma(j); this arc has cost mu 
       -explore unseen neighboors VarNodes of IneqNode sigma(j); arc between sigma(j) and l
         has cost -mu + x_l + A_{sigma(j),l} - slack_{sigma(j)}

*)
    let finished = ref false in
    while (not (!finished)) do
      let max_j = find_max None 0 in
      match max_j with
        | None -> finished := true
        | Some j ->
            (* Format.printf "looking at var node %i\n" j;  *)
            if (Array.get simplet.var_seen j) then assert false;
            Array.set simplet.var_seen j true;
            let (sigma_j, sign, entry) = 
              match Array.get simplet.max_permutation j with
                | None -> assert false
                | Some a -> a 
            in
            (* Format.printf "ie Ineq node %i\n" sigma_j; *)
            let (j_sign, j_entry) = match  Array.get simplet.dual_slacks j with
              | None -> assert false
              | Some d -> d
            in
            (* Format.printf "value at var node %i %s\n" j (Entries.to_string j_entry); *)
            let slack_sigma_j = 
              match Array.get simplet.arg_slacks sigma_j with
                | ( var_slack ,  entry_slack ) :: _   -> 
                    LP.compute_entry_plus_var entry_slack var_slack simplet.point
                | _ -> assert false
            in

            let entry_sigma_j = Entries.add j_entry mu in
            let sign_sigma_j = match (j_sign, sign) with
              | (Pos, Pos) | (Neg, Neg) -> Pos
              | _ -> Neg
            in
            Array.set simplet.reduced_costs sigma_j (Some (sign_sigma_j, entry_sigma_j));
            let visit_neigboor (arc_index, arc_sign, arc_entry) =
              match arc_index with
                | Affine -> ()
                | Var arc_var_index ->
                    if not (Array.get simplet.var_seen arc_var_index) then 
                      let x_l = Array.get simplet.point arc_var_index in
                      let new_dist = Entries.sum
                        (neg_mu :: x_l :: arc_entry :: (Entries.neg slack_sigma_j) :: entry_sigma_j :: []) in
                      let new_sign = match (sign_sigma_j, arc_sign) with
                        | (Pos, Neg) | (Neg, Pos) -> Pos
                        | _ -> Neg
                      in
                      (*
                      let s = match arc_sign with | Pos -> "pos" | Neg -> "neg" in
                      let arc_weight =
                        Entries.sum
                          (neg_mu :: x_l :: arc_entry :: (Entries.neg slack_sigma_j) ::  []) in
                      Format.printf " arc Ineq %i to var %i with weigth %s %s\n"sigma_j arc_var_index s (Entries.to_string arc_weight); *)
                    (* Format.printf " arc Ineq %i to var %i with weigth %s %s\n"sigma_j arc_var_index s (Entries.to_string arc_entry); *)
                      let better =
                        match (Array.get simplet.dual_slacks arc_var_index) with
                          | None -> true
                          | Some (current_sign, current_entry) ->
                              (* Format.printf "  var %i: current val is %s; new_dist is %s \n" arc_var_index (Entries.to_string current_entry) (Entries.to_string new_dist); *)
                              match (Entries.compare current_entry new_dist) with
                                | 1 | 0 -> false
                                | -1 -> true
                                | _ -> assert false
                      in
                      if ( better ) then 
                        begin
                          (* 
                          let ss = match new_sign with | Pos -> "pos" | Neg -> "neg" in
                        Format.printf " set dual slack %i to %s %s\n" arc_var_index ss (Entries.to_string new_dist); *)
                      Array.set simplet.dual_slacks arc_var_index (Some (new_sign, new_dist))
                        end
            in
            let arcs = LP.get_row simplet.lp (Ineq sigma_j) in
            List.iter visit_neigboor arcs
    done;


    (* rescale reduced costs *)
    for i = 0 to (nb_ineq -1) do
       let slack_i = 
        match Array.get simplet.arg_slacks i with
          | ( var_slack ,  entry_slack ) :: _ -> 
              (*
              let var_s =
              match var_slack with
                |Affine ->  "affine"
                | Var j -> Format.sprintf "var %i" j
              in
              Format.printf "slack %i; on var %s with entry %s" i var_s (Entries.to_string entry_slack); *)
              LP.compute_entry_plus_var entry_slack  var_slack simplet.point
          | _ -> assert false
       in
       (* Format.printf " val = %s\n"  (Entries.to_string slack_i); *)
       let z_i = Array.get simplet.reduced_costs i in
       match z_i with
         | None -> ()
         | Some (sign, value) -> 
             let rescaled_red_cost = Entries.add value (Entries.neg slack_i) in
             Array.set simplet.reduced_costs i (Some (sign, rescaled_red_cost))
    done;

    ()


  (* pivot simplet i_out
     
     before calling:
     -simplet.point is a basic point for basis I
     -i_out in I
     -simplet.tangent_digraph is the tangent digraph at simplet.point
     -simplet.arg_slacks is the var/arg_slack graph at simplet.point
     -simplet.reduced_costs is the reduced costs at simplet.point
     
     after_calling:
     -simplet.point is the basic point at the other end of the tropical edge
     defined by I \ {i_out}
     -simplet.tangent_digraph is the tangent digraph at simplet.point
     -simplet.arg_slacks is the var/arg_slack graph at simplet.point
     -simplet.reduced_costs is the reduced costs at simplet.point
      *)
  let pivot simplet i_out =
   (* let dim = LP.dim simplet.lp in*)
    let nb_ineq = LP.nb_ineq simplet.lp in
    if not  (TangentDigraph.is_hyp_node simplet.tangent_digraph i_out) then
      invalid_arg("Error, simplet.pivot: input index does not belong to the basis");
    
    (* init ineq status *)
    for i = 0 to (nb_ineq -1) do
      if (TangentDigraph.is_hyp_node simplet.tangent_digraph i) then
        Array.set simplet.ineq_status i BreakHyp
      else
        Array.set simplet.ineq_status i EntHyp
    done;

    (* mark hyp node as leaving the basis *)
    Array.set simplet.ineq_status i_out Inactive;

    (* find unique arc incoming to hyp node i_out *)
    let hyp_node_i_out = TangentDigraph.get_ineq_node simplet.tangent_digraph i_out in
    let (incoming,_) = List.partition (fun (_,sign,_) -> match sign with
      | Pos -> true
      | Neg -> false) hyp_node_i_out
    in
    let direction_start_node =
      match incoming with
        | (var_index,_, _)::[] -> VarNode var_index
        | _ -> invalid_arg("Error, simplet.pivot: arg pos of hyp node i_out is not a singleton")
    in 
  
    (* remove hyp node i_out from tangent digraph *)

    (* TangentDigraph.print simplet.tangent_digraph; *)
    TangentDigraph.remove_arcs_of_node simplet.tangent_digraph (IneqNode i_out);    
    (* TangentDigraph.print simplet.tangent_digraph;  *)

   
    (* compute break hyp and direction *)
    (* traversal of tangent digraph from VarNode direction_start_node
         visited VarNodes forms the direction
         visited HypNodes are breakHyp
      *)
    let compute_break_hyp_and_direction direction node_index = 
      match node_index with 
        | IneqNode i -> 
            begin
              (* Format.printf "ineq %i\n" i; *)
              match Array.get simplet.ineq_status i with
                | BreakHyp -> Array.set simplet.ineq_status i Basis
                | EntHyp -> (* Format.printf "EntHyp %i\n" i; *)
invalid_arg("Error, simplet.pivot: error while initializing BreakHyp, found an hyp node which is not BreakHyp")
                | Basis -> (* Format.printf "Basis %i\n" i; *)
invalid_arg("Error, simplet.pivot: error while initializing BreakHyp, found an hyp node which is not BreakHyp")
                | _ -> 
                    (* Format.printf "ineq %i\n" i; *)
invalid_arg("Error, simplet.pivot: error while initializing BreakHyp, found an hyp node which is not BreakHyp")
            end;
            direction
        | VarNode var_index ->
            (*
            begin
            match var_index with 
              | Affine -> Format.printf "Affine\n"
              | Var j -> Format.printf "Var %i\n" j
            end; *)
            var_index::direction
    in
    let direction = TangentDigraph.dfs_fold_acyclic_graph simplet.tangent_digraph 
      compute_break_hyp_and_direction  [] direction_start_node in
    simplet.direction <- direction;

    (*
    Format.printf "direction: \n";
    List.iter (fun var_index -> match var_index with
      | Affine -> Format.printf "affine"
      | Var j -> Format.printf "Var %i, " j
    ) direction;
    Format.printf "\n\n";

    Format.printf "break hyp: \n";
    Array.iteri (fun i status -> 
      match status with
        | BreakHyp -> Format.printf "%i, "i
        | _ -> ()
    ) simplet.ineq_status;
    Format.printf "\n\n";
    *)

    (* compute ent hyp *)
    (* remove Ineq i from EntHyp if  argmax( W^+_ij + x_j) intersects direction *)
    List.iter (fun var_index ->
      let list = match var_index with
        | Affine -> simplet.affine_var_in_arg_slack
        | Var j -> Array.get simplet.var_in_arg_slack j
      in
      List.iter (fun ineq_index -> 
        match (Array.get simplet.ineq_status ineq_index ) with
          | EntHyp -> Array.set simplet.ineq_status ineq_index Inactive
          | Inactive -> assert (ineq_index = i_out)
          | BreakHyp -> assert false
          | Basis -> ()
      ) list
    )
      direction;
      
    (*
    Format.printf "ent hyp: \n";
    Array.iteri (fun i status -> 
      match status with
        | EntHyp -> Format.printf "%i, "i
        | _ -> ()
    ) simplet.ineq_status;
    Format.printf "\n\n";
    *)

    (* compute arg_lambda = i -> argmax_{j in direction} (|W_ij| + x_j) *) 
    for i = 0 to (nb_ineq - 1) do
      Array.set simplet.arg_lambdas i [];
    done;
    
    List.iter ( fun var_index -> 
      let col_j = LP.get_col simplet.lp var_index in
      let compute_args  (row_index, sign, entry) =
        match row_index with
          | Objective -> ()
          | Ineq ineq_index -> 
              let old_arg = Array.get simplet.arg_lambdas ineq_index in
              let value = LP.compute_entry_plus_var entry var_index simplet.point in
              let new_arg =
                match old_arg with
                  | [] -> (var_index, sign, entry)::[]
                  | (old_var_index, old_sign, old_entry)::_ -> 
                      let old_value =  LP.compute_entry_plus_var old_entry old_var_index simplet.point in
                      begin
                        match (Entries.compare value old_value) with
                          | 0  -> (var_index, sign, entry)::old_arg
                          | 1  -> (var_index, sign, entry)::[]
                          | -1 ->  old_arg
                          | _  -> assert false
                      end
              in    
              Array.set simplet.arg_lambdas ineq_index new_arg
      in   
      List.iter compute_args col_j 
    ) direction;

    (*
    Format.printf "lambdas: \n";
    for i = 0 to (nb_ineq - 1) do
      Format.printf "Ineq %i: " i;
      let arg = Array.get simplet.arg_lambdas i in
      let lambda = bound_on_length_by_ineq simplet i in
      begin
        match lambda with
          | None -> Format.printf "+oo, "
          | Some l -> Format.printf "%s, " (Entries.to_string l)
      end;
            
      begin
        match arg with 
          | [] -> assert (lambda = None)
          | (var_index, sign, entry)::_ -> 
              begin
                List.iter (fun (var_index, sign, _) -> 
                  begin
                    match var_index with
                      | Affine -> Format.printf "Affine "
                      | Var j -> Format.printf "Var %i " j
                  end;
                  begin 
                    match sign with
                      | Pos -> Format.printf "Pos,"
                      | Neg -> Format.printf "Neg,"
                  end                
                ) arg
              end;
      end;
      Format.printf "\n"
    done;
    *)

    let rec traverse_tropical_edge nb_ordinary_segment = 
      if nb_ordinary_segment > (LP.dim simplet.lp) then invalid_arg("Error, simplet.pivot: visited more ordinary segment than dimension");
      
      let i_ent = traverse_ordinary_segment simplet in
      match i_ent with
        | None ->   traverse_tropical_edge (nb_ordinary_segment+1) 
        | Some i -> 
            let arg_lambda = Array.get simplet.arg_lambdas i in
            let (out_arc_var_index, out_arc_sign, out_arc_entry) =
              match arg_lambda with
                | [] -> assert false
                |  _ :: _ :: _ -> invalid_arg ("Error, simplet.pivot: arriving at new basic point, length = lambda_i where arg_lambda_i is not a singleton.")
                |  (var_index, Pos, entry) :: [] -> assert false
                |  (var_index, Neg, entry) :: [] -> (var_index, Neg, entry)
            in
            TangentDigraph.add_arc simplet.tangent_digraph
              out_arc_var_index i out_arc_sign out_arc_entry;
      
            let arg_slack = Array.get simplet.arg_slacks i in
            let (in_arc_var_index, in_arc_entry) = 
              match arg_slack with
                | [] | _ :: _ :: _ -> assert false
                | (var_index, entry) :: [] -> (var_index, entry)
            in
            TangentDigraph.add_arc simplet.tangent_digraph in_arc_var_index i Pos in_arc_entry;
              
    in
    traverse_tropical_edge 0;


    (* update arg_slacks *)
    compute_arg_slacks_pos simplet;

    compute_reduced_costs simplet;

   (* Format.printf "\n\n Pivot finished \n"; *)
    ()



  (* init lp basic_point
     create a simplet for lp at basic_point

     -basic_point must be a basic point for lp
     

     after_calling:
     -simplet.point = basic_point
     -simplet.tangent_digraph is the tangent digraph at simplet.point
     -simplet.arg_slacks is the var/arg_slack graph at simplet.point
     -simplet.reduced_costs is the reduced costs at simplet.point
      *)
  let init lp  basic_point =
    let nb_ineq = LP.nb_ineq lp in
    let dim = LP.dim lp in

    (* check that each row in lp has at least one non null coefficient *)
    for i = 0 to (nb_ineq -1) do
      let w_i = LP.get_row lp (Ineq i) in
      match w_i with 
        | [] ->  invalid_arg ("Error, simplet.init: all coefficients are null in the inequality "^(string_of_int i)^". At least one non-null coefficient is required. ")
        | _ -> ()
    done;

(*
    (* check that rhs is non null *)
    for i = 0 to (nb_ineq -1) do
      let w_i = LP.get_row lp (Ineq i) in
      let b_i_exists = List.exists ( fun (var_index, sign, entry) -> 
        match  var_index with
          | Affine -> true
          | _ -> false
      ) w_i
      in
      if not b_i_exists then  invalid_arg ("Error, simplet.init: b_i is null for inequality "^(string_of_int i)^". The vector b must have non null entries. ")
    done;
*)

    (* build tangent digraph *)
    let tangent_digraph = TangentDigraph.compute lp basic_point in

    (* check that the given point is a basic point*)
    if not (TangentDigraph.is_basic_point tangent_digraph) then
      invalid_arg ("Error, simplet.init: input point is not a basic point. ");
    
    (* check if basic point is feasible *)
    if not (LP.is_point_feasible lp basic_point) then
      invalid_arg ("Error, simplet.init: input point is not feasible. ");


    (* init ineq_status *)
    let ineq_status = Array.make nb_ineq Inactive in
    for i = 0 to (nb_ineq -1) do
      if (TangentDigraph.is_hyp_node tangent_digraph i) then
        Array.set ineq_status i Basis
    done;


    let simplet =
 { lp = lp;  point = basic_point; tangent_digraph = tangent_digraph;
   ineq_status = ineq_status;

   arg_slacks = Array.make nb_ineq []; 
   var_in_arg_slack = Array.make dim [];
   affine_var_in_arg_slack = [];

   direction = [];
   arg_lambdas = Array.make nb_ineq []; 

   max_permutation = Array.make dim None;
   reduced_costs = Array.make nb_ineq None;
   dual_slacks= Array.make dim None;
   var_seen = Array.make dim false;
 }
    in
    compute_arg_slacks_pos simplet;
    compute_reduced_costs simplet;
    simplet


  let myprintf outchannel =
    match outchannel with
      | None -> Printf.ifprintf stdout
      | Some c -> Printf.fprintf c

    

  let solve simplet pivoting_rule outchannel =
    let nb_iter = ref 1 in
    let is_optimal = ref false in
    while not (!is_optimal) do
      myprintf outchannel "\niteration %i\n" (!nb_iter);
      print simplet outchannel;
      let leaving_ineq = pivoting_rule simplet in
      match leaving_ineq with
        | None -> is_optimal := true
        | Some i -> 
            begin
              myprintf outchannel "\npivoting on %i\n" i;
              pivot simplet i;
              nb_iter := (!nb_iter) +1;
            end
            
    done;

   

    

end
   
