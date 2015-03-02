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

(** Perturbed tropical linear programs

Given a linear program with entries in a group {b G}, construct phaseI and phaseII linear programs with entries in F*G*H where
- F is the additive group of integers
- H is the additive group of sequences of integers, ordered lexicographically
- F*G*H is ordered lexicographically


*)
module type PertLP =
sig
  include LinearProg.T
  (**  Module of linear programs that can be perturbed by this module  *)
  module LP : LinearProg.T
    
  (** {b phaseI lp}
     construct a phaseI linear program for {b lp}; returns the phaseI program along with an initial basic point

   the construction of phaseI is detailed in Section 4.4.2 of
      {e Tropical aspects of linear programming}, Pascal Benchimol, PhD Thesis, 2014
    
  *)
  val phaseI : LP.t -> (t * Entries.t array) 

  (** {b phaseII lp} 
      construct a pertubed version of {b lp} as  detailed in Section 4.4.1 of
      {e Tropical aspects of linear programming}, Pascal Benchimol, PhD Thesis, 2014
    
  *)
  val phaseII : LP.t -> t

  (** {b project (f,g,h)}
      returns g if f==0
              -oo if f <0
              raise Pinfty exception if f > 0
  *)
  val project : Entries.t -> LP.Entries.t option

  (** index of the row that enforces upper bounds in phaseII *)
  val phaseII_upperbound_row : LP.t -> int
  (** index of the row that enforces a lower bound on the phaseI extra variable *)
  val phaseI_infeasibility_var_lower_bound_row : LP.t -> int

end

module Make (LP : LinearProg.T)  :  (PertLP with module LP = LP) =
struct
  module F = Group.Int
  module G =  LP.Entries 
  module H =  Group.MakeCartesianPowerSparse(Group.Int) 
  module PertG = Group.MakeCartesianTriple (F) (G) (H) 

  include Make(PertG)
  module LP = LP

  exception Pinfty
  let project fgh = 
    let f = PertG.first fgh in
    match F.compare f F.zero with
      | 1 -> raise Pinfty
      | 0 -> Some (PertG.second fgh)
      | -1 -> None
      | _ -> assert false

  (* If an affine entry of a row is -oo,
     will be set to the result of this function in phaseI and phaseII *)
  let affine_perturbation row_index lp =
    assert (row_index >=0);
    assert (row_index < (LP.nb_ineq lp));
    let f = -1 in
    let fgh = PertG.from_entries f G.zero H.zero in
    fgh

  (* lower bounds on original variables in phaseI and phaseII 
     F coordinate must be stricly smaller than F coordinate of any affine_perturbation
  *)
  let lower_bound col_index lp =
    assert (col_index >= 0);
    assert (col_index < (LP.dim lp));
    let f = -2 in
    let fgh = PertG.from_entries f G.zero H.zero in
    fgh

  (* lower bound for phaseI extra variable
     F coordinate must be stricly smaller than F coordinate of any affine_perturbation and any lower_bound
  *)
  let phaseI_lower_bound =
    let f = -3 in
    let fgh = PertG.from_entries f G.zero H.zero in
    fgh
    
  (*  upper bounds on every variable in phaseI and phaseII
      F coordinate must be positive
  *)
  let upper_bound  =
    let f = 1  in
    let fgh = PertG.from_entries f G.zero H.zero in
    fgh

  (* Set the H coordinate of coefficient (i,j) of a matrix
     must ensure that the resulting matrix is generic for any minor polynomial
     if the pivoting rule use other polynomial than minors, must also ensure genericity for these polynomials
  *)
  let epsilon_perturbation_coeff i j nb_columns =
    assert(j >= 0);
    assert (j < nb_columns);
    assert (i >=0);
    H.from_list ((i*(nb_columns)+j, 1)::[])

(* auxilliary perturbation functions, built from the functions above *)
      
  let rec epsilon_perturb_row old_row new_row row_index nb_columns=
    match old_row with
      | [] -> new_row
      | (col_index, sign, fgh) :: tail ->
          let j = match col_index with 
            | Var j -> j
            | Affine -> nb_columns -1
          in
          let f = PertG.first fgh in
          let g = PertG.second fgh in
          let h = epsilon_perturbation_coeff row_index j nb_columns in
          let pert_fgh = match sign with
            | Pos -> PertG.from_entries f g h 
            | Neg -> PertG.from_entries f g (H.neg h)
          in
          let new_coeff = match sign with
            | Pos -> (col_index, sign, pert_fgh) 
            | Neg -> (col_index, sign, pert_fgh)
          in
          epsilon_perturb_row tail (new_coeff::new_row) row_index nb_columns
            
  let epsilon_perturbation rows first_row_index  nb_columns =
    let rec epsilon_perturbation_aux old_rows new_rows i =
      match old_rows with
        | [] -> new_rows
        | row :: tail -> 
            let new_row = epsilon_perturb_row row [] i nb_columns in
            epsilon_perturbation_aux tail (new_row::new_rows) (i+1)
    in
    epsilon_perturbation_aux rows [] first_row_index

  let rec process_row new_row old_row =
    match old_row with
      | [] -> new_row
      | (col_index, sign, g):: tail ->
          let f = F.zero in
          let fgh = PertG.from_entries f g H.zero in
          process_row ((col_index, sign, fgh)::new_row) tail
  
 
  let rec new_rows_builder new_rows i lp =
    let rec contains_affine row =
      match row with
        | [] -> false
        | (Affine, _,_)::_ -> true
        | _ :: tail -> contains_affine tail  
    in  
    if i >= (LP.nb_ineq lp) then new_rows
    else
      let old_row = LP.get_row lp (Ineq i) in
      let processed_row = process_row  [] old_row in
      let new_row = 
        if (contains_affine old_row) then processed_row
        else 
          let fgh = affine_perturbation i lp in
          (Affine, Pos, fgh) :: processed_row
      in
      new_rows_builder (new_row::new_rows) (i+1) lp
  
  let infinity_plane_row lp = 
    let rec infinity_plane_row_builder row j  =
      if j >= LP.dim lp then row
      else 
        infinity_plane_row_builder ((Var j, Neg, PertG.zero)::row) (j+1)
    in
    let fgh =  upper_bound  in
    let affine_coord = (Affine, Pos, fgh) in
    infinity_plane_row_builder (affine_coord::[]) 0 

  let rec lower_bounds_builder new_rows j lp =
    if j >= (LP.dim lp) then new_rows
    else
      let fgh = lower_bound j lp in
      let row =  (Affine, Neg, fgh) :: (Var j, Pos, PertG.zero) :: [] in
      lower_bounds_builder ( row::new_rows) (j+1) lp
(* in
  let lower_bounds_rows lp =
    lower_bounds_builder [] 0
*)

  let phaseI lp =

    let processed_input_rows = new_rows_builder [] 0 lp in

    let infeasibility_var_index = (LP.dim lp) in
    let rec add_infeasibility_var old_rows new_rows =
      match old_rows with
        | [] -> new_rows
        | row::tail -> 
            let new_coeff = (Var infeasibility_var_index, Pos, PertG.zero) in
            let new_row = new_coeff::row in
            add_infeasibility_var tail (new_row::new_rows) 
    in
    let w = add_infeasibility_var processed_input_rows [] in
    let ww = List.rev w in
    let lower_bounds = lower_bounds_builder ww 0 lp in

    let infeasibility_var_lower_bound =
(Affine, Neg, phaseI_lower_bound) :: (Var infeasibility_var_index, Pos, PertG.zero)::[] in


    let phaseII_infinity_plane = infinity_plane_row lp in
    let infinity_plane = (Var infeasibility_var_index, Neg, PertG.zero):: phaseII_infinity_plane in

    let matrix = (infinity_plane :: infeasibility_var_lower_bound :: lower_bounds) in
    let nb_columns = (LP.dim lp)+2  in
    let m = List.rev matrix in

    let perturbed_m = epsilon_perturbation m  1 nb_columns in
    let perturbed_matrix = List.rev perturbed_m in
(*Format.printf "matrix perturbed\n";*)
    let objective_row =  (Var infeasibility_var_index, Pos, PertG.zero)::[] in
    let perturbed_objective = epsilon_perturb_row objective_row [] 0 nb_columns in
    let phaseI_lp = init ((LP.dim lp)+1) perturbed_objective perturbed_matrix
    in
    let initial_basic_point  = Array.make ((LP.dim lp)+1) PertG.zero in
    for j=0 to (LP.dim lp)-1 do
      let i =  1+ (LP.nb_ineq lp) + j          in
      let l =lower_bound j lp
(*        if j < (LP.dim lp) then lower_bound j lp 
        else upper_bound*)
      in
      let pert = 
        H.add
          (H.neg (epsilon_perturbation_coeff i j nb_columns))
          (H.neg (epsilon_perturbation_coeff i (nb_columns-1) nb_columns))
      in
      let pertl = PertG.add l
        (PertG.from_entries F.zero G.zero pert) in
      (*Format.printf "%i: %s\n" j (PertG.to_string pertl);*)
      Array.set initial_basic_point j pertl
    done;
    let j = (LP.dim lp) in
    let i= 1+ (LP.nb_ineq lp) + (LP.dim lp) +1 in
    let l = upper_bound in
    let pert = H.add 
          ((epsilon_perturbation_coeff i j nb_columns))
          ((epsilon_perturbation_coeff i (nb_columns-1) nb_columns))
    in
    let pertl = PertG.add l
        (PertG.from_entries F.zero G.zero pert) in
    (*Format.printf "%i: %s\n" j (PertG.to_string pertl);*)
    Array.set initial_basic_point j pertl;
    (phaseI_lp, initial_basic_point)


  (* added first lower bounds, then processed input matrix, then infinity plane
the call to epsilon_perturbation reverse the order, so the infinity plane row is the last row of "perturbed_matrix" and it is indexed by m+n
*)
  let phaseI_infeasibility_var_lower_bound_row lp =  ((LP.dim lp) + (LP.nb_ineq lp))

  let phaseII lp =


    let processed_input_rows = new_rows_builder [] 0 lp in

    let w =  processed_input_rows in

    let lower_bounds = lower_bounds_builder w 0 lp in


    let infinity_plane = infinity_plane_row lp in

    let matrix =  lower_bounds in
    let nb_columns = (LP.dim lp)+2  in
    let m = List.rev matrix in
    let perturbed_m = epsilon_perturbation m  1 nb_columns in
    let pert_infinity_plane = epsilon_perturbation (infinity_plane::[]) ((LP.dim lp)+(LP.nb_ineq lp) + 2) nb_columns in
    let pert_m = List.append pert_infinity_plane perturbed_m in
    let perturbed_matrix = List.rev pert_m in
    let objective_row = process_row [] (LP.get_row lp Objective) in
    let perturbed_objective = epsilon_perturb_row objective_row [] 0 nb_columns in
    let phaseII_lp = init ((LP.dim lp)) perturbed_objective perturbed_matrix in
      phaseII_lp

      
      (* added first lower bounds, then processed input matrix, then infinity plane
the call to epsilon_perturbation reverse the order, so the infinity plane row is the last row of "perturbed_matrix" and it is indexed by m+n
*)
  let phaseII_upperbound_row lp =  ( (LP.dim lp) + (LP.nb_ineq lp))


end


