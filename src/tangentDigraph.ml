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

open LinearProg


type ineq_index_t = int
type var_index_t = col_index_t 
    
type node_index_t = VarNode of var_index_t | IneqNode of ineq_index_t

(** Tangent digraph of a tropical linear program at a point

Given a tropical linear program in dim n with m inequalities of matrix 
|c .|
|A b|
 and a point x.

Tangent digraph is a bipartite directed graph between variable nodes (Affine or Var j, with 0 <= j <= n-1) and inequality nodes (IneqNode i with 0 <= i <= m-1).

IneqNode i has arcs only if the inequality i is saturated at x; ie if
max ( a^+_i1 + x_1 , ..., a^+_in + x_n, b^+_i) = max ( a^-_i1 + x_1 , ..., a^-_in + x_n, b^-_i)
In this case, IneqNode i has incoming arcs from VarNodes 
argmax ( a^+_i1 + x_1 , ..., a^+_in + x_n, b^+_i)
and outgoing arcs to VarNodes
argmax( a^-_i1 + x_1 , ..., a^-_in + x_n, b^-_i)

*)

module type T =
sig
  module Entries : Group.Ordered
  module LP: LinearProg.T with module Entries = Entries
  type t

(** compute lp x
    compute the tangent digraph of lp at x
    
    input must satisfy:
    - dim lp = length x
    - for all i in [nb_ineq lp], the vector (a^+_i b^+_i a^-_i b^-_i ) have at least one non null coefficient

*)

  val compute : LP.t ->  (Entries.t array) -> t

  val nb_connected_component : t -> int
  val is_basic_point : t -> bool
  val is_breakpoint : t -> bool

  (** return  true if Ineq i is saturated by x in lp*)
  val is_hyp_node : t -> ineq_index_t -> bool

  val get_ineq_node : t -> ineq_index_t -> (var_index_t * sign_t * Entries.t) list
  val get_var_node : t -> var_index_t -> (ineq_index_t * sign_t *  Entries.t) list

  val remove_arc: t -> var_index_t -> ineq_index_t -> unit
  val add_arc: t -> var_index_t -> ineq_index_t -> sign_t -> Entries.t -> unit


  val remove_arcs_of_node : t -> node_index_t -> unit


    (** infix fold on a depth first traversal
        WARNING: apply only on acyclic graphs

        if we arrive at node v from node parent_v, and v has neighboors r_1, ..., r_d (parent_v excluded) then return
        (dfs_fold f ( ... (dfs_fold f (f acc v) r_1) ... ) r_d)
        
    *)
  val dfs_fold_acyclic_graph : t -> ('a -> node_index_t -> 'a) -> 'a -> node_index_t -> 'a

  val print : t -> out_channel -> unit
end

module Make (LP : LinearProg.T)
 : (T with module Entries = LP.Entries with module LP = LP )  =
struct
  module Entries = LP.Entries
  module LP = LP

  (* var_nodes.(i) : set of arcs connected to VarNode i; a Pos sign indicates incoming arc, Neg sign outgoing; entry is the coefficient w_ij (a_ij or b_i) in the linear program
    affine_var_node : arcs of VarNode Affine
     ineq_nodes.(i) : arcs of IneqNode i
  *)
  type t = { 
    var_nodes : ((ineq_index_t * sign_t * Entries.t) list) array;
    mutable affine_var_node: (ineq_index_t  * sign_t * Entries.t) list;
    ineq_nodes : ((var_index_t * sign_t * Entries.t) list) array;
  }


  let get_ineq_node tangent_digraph ineq_index =
    try
      Array.get tangent_digraph.ineq_nodes ineq_index
    with | Invalid_argument(s) -> invalid_arg ("Error, tangent_digraph.get_ineq_node: input index out of bounds")

  let get_var_node tangent_digraph var_index =
    try
      match var_index with
        | Affine -> tangent_digraph.affine_var_node 
        | Var j -> Array.get tangent_digraph.var_nodes j
    with | Invalid_argument(s) -> invalid_arg ("Error, tangent_digraph.get_var_node: input index out of bounds")




  let is_hyp_node  tangent_digraph ineq_index =
    let  ineq_node =
      try Array.get tangent_digraph.ineq_nodes ineq_index 
      with | Invalid_argument(s) -> invalid_arg ("Error, tangent_digraph.is_hyp_node: input index out of bounds")
    in
    let rec is_hyp_node_aux nb_pos nb_neg arcs =
      if nb_pos >1 or nb_neg > 1 then false
      else
        match arcs with 
          | [] -> if ((nb_pos =1) && (nb_neg = 1)) then true
            else false
          | (_, Pos,_)::tail -> is_hyp_node_aux (nb_pos+1) nb_neg tail
          | (_, Neg,_)::tail -> is_hyp_node_aux nb_pos (nb_neg+1) tail
    in
    is_hyp_node_aux 0 0 ineq_node


  let nb_hyp_nodes tangent_digraph = 
    let rec counter count ineq_index=
      if (ineq_index >= Array.length tangent_digraph.ineq_nodes) then
        count
      else if (is_hyp_node tangent_digraph ineq_index) then counter (count+1) (ineq_index+1)
      else counter count (ineq_index+1)
    in
    counter 0 0



  let contains_arc tangent_digraph var_index ineq_index =
    let ineq_node = Array.get tangent_digraph.ineq_nodes ineq_index
    in
    let rec contains_arc_aux arcs =
      match arcs with
        | [] -> false
        | (iter_var_index,_,_)::_ when iter_var_index = var_index -> true
        | head :: tail ->  contains_arc_aux tail
    in
    if (contains_arc_aux ineq_node) then
      true
    else false
      

  

  let add_arc tangent_digraph var_index ineq_index sign entry=
    let rec arc_exists arcs node_index =
      match arcs with
        | [] -> ()
        | (iter_node_index, _, _) :: _ when iter_node_index = node_index -> 
            invalid_arg  ("Error, TangentDigraph.add arc: trying to add an arc which already exists")
        | _ :: tail -> arc_exists tail node_index
    in
    let var_node = 
      match var_index with
        | Affine -> tangent_digraph.affine_var_node
        | Var j -> (Array.get tangent_digraph.var_nodes j)
    in
    arc_exists var_node ineq_index;
    let new_var_node = (ineq_index, sign, entry) :: var_node in
    begin
      match var_index with
        | Affine -> tangent_digraph.affine_var_node <- new_var_node
        | Var j -> Array.set tangent_digraph.var_nodes j new_var_node
    end;
    
      let ineq_node = Array.get tangent_digraph.ineq_nodes ineq_index in
      arc_exists ineq_node var_index;
      let new_ineq_node =  (var_index, sign, entry) :: ineq_node in
      Array.set tangent_digraph.ineq_nodes ineq_index new_ineq_node
         
  let remove_arc tangent_digraph var_index ineq_index =
    let remove_arc_from_list node_index list =
      let (arc_to_remove, remaining_arcs) = List.partition (
              fun (iter_node_index, iter_sign,iter_entry) -> iter_node_index = node_index ) list
      in
      match arc_to_remove with
        | _ :: [] -> remaining_arcs
        | [] -> invalid_arg  ("Error, TangentDigraph.remove arc: trying to remove an arc which doest not exists")
        | _ -> invalid_arg  ("Error, TangentDigraph.remove arc: mutiple occurence of the same arc")
    in
    let var_node = 
      match var_index with
        | Affine -> tangent_digraph.affine_var_node
        | Var j -> (Array.get tangent_digraph.var_nodes j)
    in
    let new_var_node = remove_arc_from_list ineq_index var_node in
    begin
      match var_index with
        | Affine -> tangent_digraph.affine_var_node <- new_var_node
        | Var j -> Array.set tangent_digraph.var_nodes j new_var_node
    end;

    let ineq_node = Array.get tangent_digraph.ineq_nodes ineq_index in
    let new_ineq_node = remove_arc_from_list var_index ineq_node in
    Array.set tangent_digraph.ineq_nodes ineq_index new_ineq_node

  let nb_var_nodes tangent_digraph =
    (Array.length tangent_digraph.var_nodes)+1

  let nb_ineq_nodes tangent_digraph =
    Array.length tangent_digraph.ineq_nodes
      

      
  let remove_arcs_of_node tangent_digraph node_index =
    let remove_var_node var_index =
      let var_node = get_var_node tangent_digraph var_index in
      List.iter (fun (ineq_index, sign, entry) -> 
      remove_arc tangent_digraph var_index ineq_index)
        var_node
    in
    let remove_ineq_node ineq_index =
      let ineq_node = get_ineq_node tangent_digraph ineq_index in
      List.iter (fun (var_index, sign, entry) -> 
        remove_arc tangent_digraph var_index ineq_index)
        ineq_node
    in
    match node_index with
      | VarNode Affine ->  remove_var_node Affine
      | VarNode (Var j) ->  remove_var_node (Var j)
      | IneqNode i  ->   remove_ineq_node i


    
    
      

  let compute lp point =
    let dim = LP.dim lp in
    let nb_ineq = LP.nb_ineq lp in

    if (Array.length point) <> dim then 
      invalid_arg ("Error while computing tangent digraph: linear program has "^(string_of_int dim)^" variables and input point has dimension "^(string_of_int (Array.length point))); 
  
    

    let var_nodes = Array.make dim [] in
    let affine_var_node = ref [] in
    let ineq_nodes = Array.make nb_ineq [] in
      

    
    (* compute hyp_nodes incoming/outgoing arcs *)
    (* z = (x 0) 
       w_i = ( a_i b_i) *)
    let rec compute_hyp_nodes ineq_index hyp_nodes =
      if ineq_index >= nb_ineq then hyp_nodes
      else
        (* arg_pos = argmax_j W^+_ij + z_j 
           arg_neg = argmax_j W^+_ij + z_j *)
        let arg = LP.compute_slack_args lp (Ineq ineq_index) point in
        
        let (pos, neg) =
          List.fold_left ( fun (pos, neg) (col_index, sign, entry) ->
            match sign with
              | Pos -> (true, neg)
              | Neg -> (pos, true)
          ) (false, false) arg
        in
        match (pos, neg) with
          | (false, _ ) -> invalid_arg  ("Error while initializing tangent digraph. Input point does not satisfy  inequality indexed by "^(string_of_int ineq_index))
          | (true, false) -> compute_hyp_nodes (ineq_index+1) hyp_nodes
          | (true, true) ->
              Array.set ineq_nodes ineq_index arg;
              compute_hyp_nodes (ineq_index+1) (ineq_index::hyp_nodes)
              

    in
    let hyp_nodes = compute_hyp_nodes 0 [] in
    
  (* compute var_nodes incoming/outgoing arcs *)
   
    let add_arcs_from_ineq ineq_index =
      let ineq_node = Array.get ineq_nodes ineq_index in
      List.iter (fun (var_index, sign, entry) ->
        let old_var_node = 
          match var_index with
            | Affine -> !affine_var_node
            | Var j -> Array.get var_nodes j
        in
        let new_var_node = (ineq_index, sign, entry) :: old_var_node in
        match var_index with
          | Affine -> affine_var_node :=new_var_node
          | Var j -> Array.set var_nodes j new_var_node

      ) ineq_node
    in
    List.iter add_arcs_from_ineq hyp_nodes;

    {var_nodes = var_nodes; ineq_nodes = ineq_nodes;  affine_var_node = !affine_var_node;}



  let dfs_fold_acyclic_graph tangent_digraph f acc start_node_index  =
    let rec dfs_acyclic_aux f acc_aux node_index parent_node_index =
      let acc1 = f acc_aux node_index in
      let visit_hypnode acc_iter  (ineq_index, sign, entry) =
        match parent_node_index with
          | Some (VarNode _) -> assert false
          | Some (IneqNode i) when (i = ineq_index) -> acc_iter
          | _ ->dfs_acyclic_aux f acc_iter (IneqNode ineq_index) (Some node_index)
      in
      let visit_varnode acc_iter  (var_index, sign, entry) = 
        match parent_node_index with
          | Some (IneqNode _) -> assert false
          | Some (VarNode v) when (v = var_index) -> acc_iter
          | _ ->
              (*Format.printf "   visit var node \n" ;*)
        dfs_acyclic_aux f acc_iter (VarNode var_index) (Some node_index)
      in
      match node_index with
        | VarNode Affine -> 
            let arcs = tangent_digraph.affine_var_node in
            List.fold_left visit_hypnode acc1 arcs
        | VarNode (Var j) -> 
            let arcs = Array.get tangent_digraph.var_nodes j in
            List.fold_left visit_hypnode acc1 arcs
        | IneqNode i -> 
            let arcs = Array.get tangent_digraph.ineq_nodes i in
            List.fold_left visit_varnode acc1 arcs
    in
    dfs_acyclic_aux f acc start_node_index None


  let depth_first_iter tangent_digraph f node_index =
    let ineq_nodes_length = Array.length tangent_digraph.ineq_nodes  in
    let var_nodes_length = Array.length tangent_digraph.var_nodes in
    
    let var_node_seen = Array.make var_nodes_length false in
    let affine_var_node_seen = ref false in
    let hyp_node_seen = Array.make ineq_nodes_length false in

    let mark_seen node =
      match node with
      | VarNode Affine -> affine_var_node_seen := true
      | VarNode (Var j) -> Array.set var_node_seen j true
      | IneqNode i -> Array.set hyp_node_seen i true
    in
    let is_seen node =
      match node with
      | VarNode Affine -> !affine_var_node_seen 
      | VarNode (Var j) -> Array.get var_node_seen j 
      | IneqNode i -> Array.get hyp_node_seen i 
    in

    let rec depth_first_iter_aux f node =
      mark_seen node;
      f node;
      let visit_hypnode (ineq_index, sign, entry) =
        if (not (is_seen (IneqNode ineq_index))) then
          depth_first_iter_aux f (IneqNode ineq_index)
      in
      let visit_varnode (var_index, sign,entry) = 
        if (not (is_seen (VarNode var_index))) then
          depth_first_iter_aux f (VarNode var_index)
      in
      match node with
        | VarNode Affine -> 
            let arcs = tangent_digraph.affine_var_node in
            List.iter visit_hypnode arcs
        | VarNode (Var j) -> 
            let arcs = Array.get tangent_digraph.var_nodes j in
            List.iter visit_hypnode arcs
        | IneqNode i -> 
            let arcs = Array.get tangent_digraph.ineq_nodes i in
            List.iter visit_varnode arcs
    in
    depth_first_iter_aux f node_index
           
  let nb_connected_component tangent_digraph =
    let ineq_nodes_length = Array.length tangent_digraph.ineq_nodes in
    let var_nodes_length = Array.length tangent_digraph.var_nodes in
    
    let var_node_seen = Array.make var_nodes_length false in
    let affine_var_node_seen = ref false in
    let ineq_node_seen = Array.make ineq_nodes_length false in

    let mark_seen node =
      match node with
      | VarNode Affine -> affine_var_node_seen := true
      | VarNode (Var j) -> Array.set var_node_seen j true
      | IneqNode i -> Array.set ineq_node_seen i true
    in
    let nb_cc = ref 0 in
    (*
    Array.iteri (fun i hyp_node ->
      if (not (Array.get hyp_node_seen i)) then
        begin
          nb_cc := !nb_cc + 1;
          Format.printf "IneqNode %i\n" i;
          depth_first_iter tangent_digraph mark_seen (IneqNode i) 
        end)
      tangent_digraph.ineq_nodes;
    *)
    Array.iteri (fun i ineq_node ->
      if (not (Array.get ineq_node_seen i)) then
        begin
          nb_cc := !nb_cc + 1;
(*          Format.printf "IneqNode %i\n" i;*)
          depth_first_iter tangent_digraph mark_seen (IneqNode i) 
        end
    ) tangent_digraph.ineq_nodes;
      
    Array.iteri (fun j var_node ->
      if (not (Array.get var_node_seen j)) then
        begin
          nb_cc := !nb_cc +1;
(*          Format.printf "VarNode %i\n" j;*)
          depth_first_iter tangent_digraph mark_seen (VarNode (Var j)) 
        end)
      tangent_digraph.var_nodes;
    if (not (!affine_var_node_seen)) then
      begin
(*        Format.printf "Affine var node\n";*)
        nb_cc := !nb_cc +1
      end;
    !nb_cc
        

  let degree_of_ineq_node tangent_digraph ineq_index =
    let ineq_node = Array.get tangent_digraph.ineq_nodes ineq_index in
    List.fold_left (fun (nb_pos, nb_neg) (_, sign, _) ->
      match sign with 
        | Pos -> (nb_pos+1, nb_neg)
        | Neg -> (nb_pos, nb_neg+1)
    ) (0,0) ineq_node



  let is_basic_point tangent_digraph =

    (* check if the number of hyperplane nodes is right *)
    let nb_hyp_nodes = nb_hyp_nodes tangent_digraph in
    let nb_var_nodes = nb_var_nodes tangent_digraph in
    let nb_ineq_nodes = nb_ineq_nodes tangent_digraph in
    if nb_hyp_nodes <> nb_var_nodes - 1 then 
      begin
        Printf.fprintf stderr "Not a tangent digraph of a basic point: nb of hyperplane nodes not equal to nb of variables.\n";
        false
      end
    
    else
      (* check that each hyperplane node has one incoming and one outgoing arc *)
      let rec is_degrees_ok ineq_index  =
        if ineq_index >= Array.length tangent_digraph.ineq_nodes then true
        else 
          let (nb_pos, nb_neg) = degree_of_ineq_node tangent_digraph ineq_index in
          match (nb_pos, nb_neg) with
            | (1,1) | (0,0) -> is_degrees_ok (ineq_index+1)
            | _ -> false
      in
      if not ( is_degrees_ok 0 ) then false
      else
    
        (* check connectedness *)
        let nb_cc = nb_connected_component tangent_digraph in
        if nb_cc = (nb_ineq_nodes - (nb_var_nodes-1) + 1) then true
        else
          begin
            Printf.fprintf stderr "Not a tangent digraph of a basic point: there is %i connected components instead of %i.\n" nb_cc (nb_ineq_nodes - (nb_var_nodes-1) + 1);
            false
          end
          

  let is_breakpoint tangent_digraph =
    (* check if the number of hyperplane nodes is right *)
    let nb_hyp_nodes = nb_hyp_nodes tangent_digraph in
    let nb_var_nodes = nb_var_nodes tangent_digraph in
    let nb_ineq_nodes = nb_ineq_nodes tangent_digraph in
    if nb_hyp_nodes <> nb_var_nodes - 2 then 
      begin
        Printf.fprintf stderr "Not a tangent digraph of a breakpoint: nb of hyperplane nodes not equal to nb of variables.\n";
        false
      end
    else

       (* check that each hyperplane node has one incoming and one outgoing arc *)
      let rec is_degrees_ok degree_three_found ineq_index  =
        if ineq_index >= Array.length tangent_digraph.ineq_nodes then degree_three_found
        else
          let (nb_pos, nb_neg) = degree_of_ineq_node tangent_digraph ineq_index in
          match (nb_pos, nb_neg, degree_three_found) with
            | (1,1, _) | (0,0, _) -> is_degrees_ok degree_three_found (ineq_index+1)
            | (2,1, true) | (1,2,true) ->
                Printf.fprintf stderr "Not a tangent digraph of a breakpoint: found two hyperplane nodes of degree 3. \n";
                false
            | (2,1, false) | (1,2, false) -> is_degrees_ok true (ineq_index+1)
            | _ -> 
                 Printf.fprintf stderr "Not a tangent digraph of a breakpoint:  hyperplane node %i is not of degree 2 or 3.\n" ineq_index;
                false
      in
      if not ( is_degrees_ok false 0 ) then
        false
      else
        (* check connectedness *)
        let nb_cc = nb_connected_component tangent_digraph in
        if nb_cc = nb_ineq_nodes - (nb_var_nodes-1) + 1 then true
        else
          begin
            Printf.fprintf stderr "Not a tangent digraph of a breakpoint: there is %i connected components instead of %i.\n" nb_cc (nb_ineq_nodes - (nb_var_nodes-1) + 1);
            false
          end
            

      
  let print tangent_digraph channel=
    Array.iteri (fun i arcs ->
      Printf.fprintf channel "hyp node %i: " i;
      let print_arc (var_index,sign,entry) =
        let var_s =
          match var_index with
            | Affine -> "affine"
            | Var j -> "var "^(string_of_int j)
        in
        Printf.fprintf channel "(%s, %s); " var_s (Entries.to_string entry)
      in
      let (incoming, outgoing) = List.partition (
        fun (_, sign, _) -> match sign with 
          | Pos -> true
          | Neg -> false
      ) arcs
      in
      Printf.fprintf channel " incoming : ";
      List.iter print_arc incoming;
      Printf.fprintf channel " outgoing :";
      List.iter print_arc outgoing;
      Printf.fprintf channel"\n"
    ) tangent_digraph.ineq_nodes
      
    
end
