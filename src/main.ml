
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

open Simplet
open LinearProg
open PerturbedLP
open Lexing



let verbose = ref false

let log_file_name = ref "log"

let input_file_name = ref ""


type solution_t = Infeasible | Unbounded | Optimum 


let main input_filename=

  let print_position  lexbuf =
    let pos = lexbuf.lex_start_p in
    Printf.sprintf  "line %d, char %d" 
      pos.pos_lnum (pos.pos_cnum - pos.pos_bol ) in
  
  (*read header of the input file *)
  let inchannel = (open_in input_filename) in
  let stdinbuf = Lexing.from_channel inchannel in


  let (numeric_name, var_names) =
    try
      let (numeric_name,var_names) = Lexer.header stdinbuf in
      (numeric_name, var_names)
    with
      | Lexer.Error msg ->
          Printf.fprintf stderr "%s: lexer error:\n%s" (print_position stdinbuf) msg;
          assert false;
  in


  let module Num = (val (Numeric.get (!numeric_name)): Numeric.T) in
  let module Parse =  Parser.Make(Num) in


(* read problem *)
  let (obj,ineq, basic_point_list) =
    try 
      Parse.main Lexer.token stdinbuf
    with
      | Lexer.Error msg ->
          Printf.fprintf stderr "at %s: lexer error:\n%s" (print_position stdinbuf) msg;
          assert false;
    (*	Printf.fprintf stderr "%s%!" msg*)
      | Parse.Error ->
          Printf.fprintf stderr "at %s: syntax error\n at '%s'\n" (print_position stdinbuf) (Lexing.lexeme stdinbuf);
          assert false;
  in

  close_in inchannel;
  print_endline "input file parsed";

  let module G = Group.Make(Num)  in
  let module LP = LinearProg.Make(G) in

  let nb_var = Hashtbl.length var_names in
  let var_names_array =  
    let array = Array.make nb_var "" in
    Hashtbl.iter ( fun  name index -> Array.set array index name) var_names ;
    array
  in
  let var_names_fun   = (fun j ->  Array.get var_names_array j) in

  (*build linear program from input *)
  let lp = LP.init ~var_names:var_names_fun nb_var obj ineq in



  let logchannel = 
    if (!verbose) then
      let c  = open_out (!log_file_name) in 
      Some c
    else
      None
  in

  let myprintf channel =
    match channel with
      | None -> Printf.ifprintf stdout
      | Some c -> Printf.fprintf c
  in
  
  myprintf logchannel "\n parsed lp\n\n";
  LP.pretty_print lp logchannel;
  
  let basic_point_given = 
    match basic_point_list with
      | [] -> false
      | _ -> true
  in


  let (solution, optimal_basic_point) =
    if basic_point_given then
      let basic_point = Array.of_list basic_point_list in
      let  module Simp= Simplet.Make(LP) in
      let phaseII_simplet = Simp.init lp basic_point in
      
      print_endline "applying tropical simplex method with given input basic point";
      myprintf logchannel  "\n------------------\napplying tropical simplex method with given input basic point\n\n";
      Simp.solve phaseII_simplet Simp.bland_rule logchannel;

      let opt = Simp.basic_point phaseII_simplet in
      let opt_projected = Array.map
        (fun  x ->  (Some x))
        opt 
      in
      (Optimum, opt_projected)
      

(* build phase I and phase II *)  
    else begin 
      
      let module PertLP = PerturbedLP.Make(LP) in
      let (phaseI_lp, basic_point) = PertLP.phaseI lp in
      
      myprintf logchannel "\n------------------\nphaseI lp:\n\n";
      PertLP.print  phaseI_lp logchannel;

      let module Simplet = Simplet.Make(PertLP) in
      let phaseI_simplet = Simplet.init phaseI_lp basic_point in
      

      print_endline "solving phaseI";
      myprintf logchannel "\n------------------\ncall simplex method on phaseI lp\n\n";
      Simplet.solve phaseI_simplet Simplet.bland_rule logchannel;
      let phaseI_opt_basic_point = Simplet.basic_point phaseI_simplet in
      
      let feasible = Simplet.basis_contains phaseI_simplet (PertLP.phaseI_infeasibility_var_lower_bound_row lp) in
      
      
      if (feasible == false) then 
        (Infeasible, Array.make 0  (Some G.zero))

      (* if found a feasible basic point for phaseII, build and solve phaseII *)
      else  begin
                        
        print_endline "solving phaseII";

        let phaseII_lp = PertLP.phaseII lp in
        myprintf logchannel "\n---------\nphaseII lp:\n\n";
        PertLP.print phaseII_lp logchannel;

        let phaseII_basic_point = Array.init (LP.dim lp)
        (fun i -> Array.get phaseI_opt_basic_point i) in
        
        let phaseII_simplet = Simplet.init phaseII_lp phaseII_basic_point in
        myprintf logchannel "\n------------------\ncall simplex method on phaseII lp\n\n";
        Simplet.solve phaseII_simplet Simplet.bland_rule logchannel;
        
        let opt_basis_contains_infinity_plane = Simplet.basis_contains phaseII_simplet (PertLP.phaseII_upperbound_row lp) in
        let infinity_plane_red_cost = Simplet.red_cost phaseII_simplet (PertLP.phaseII_upperbound_row lp) in
        
        if (opt_basis_contains_infinity_plane == false) then 
           let opt = Simplet.basic_point phaseII_simplet in
           let opt_projected = Array.map
             (fun x ->  PertLP.project x )
             opt;
           in
           (Optimum, opt_projected)
        else
          let unbounded =
            match infinity_plane_red_cost with              
              | Some (Pos, _) -> true
              | None  -> false
              | Some (Neg, _) -> assert false
          in
          if (unbounded) then
            (Unbounded, Array.make 0 (Some G.zero))
          else
            begin
              myprintf logchannel " ===\n last pivot on %i to obtain a point with finite entries\n ===\n" (PertLP.phaseII_upperbound_row lp) ;
              
              Simplet.pivot phaseII_simplet (PertLP.phaseII_upperbound_row lp);
              Simplet.print phaseII_simplet logchannel;
              
              let opt = Simplet.basic_point phaseII_simplet in
              let opt_projected = Array.map
                (fun x ->  PertLP.project x )
                opt;
              in
              (Optimum, opt_projected)
            end
              
      end
    end 
  in

  match solution with
    | Infeasible -> 
        begin
          print_endline "Infeasible";
          myprintf logchannel " ===\n Infeasible\n ===\n";  
        end
    | Unbounded ->
        begin
          print_endline "Unbounded";
          myprintf logchannel " ===\n Unbounded\n ===\n";
        end
    | Optimum ->
        begin
          print_endline "Optimal basic point found";
          let solution_printer channel i x_i = 
            let var_name = var_names_fun i in
            let value = match x_i with
              | None -> "-oo"
              | Some x -> G.to_string x
            in
            Printf.fprintf channel "%s: %s\n" var_name value
          in
          Array.iteri 
            (fun i x_i -> solution_printer stdout i x_i) 
            optimal_basic_point;
          myprintf logchannel " ===\n Optimal basic point found\n ===\n";
          begin
            match logchannel with 
              | None -> ()
              | Some c ->   
                  begin
                    Array.iteri 
                      (fun i x_i -> solution_printer c i x_i) 
                      optimal_basic_point;
                    close_out c;
                  end;
          end;
        end;
  ()


let _ = 
begin
let speclist = [("-log", Arg.String (fun s -> verbose:= true; log_file_name := s; )  , "Enables verbose mode and writes log in given filename");
]
in let usage_msg = "Solver for tropical linear programs based on the tropicalization of the simplex method."
in Arg.parse speclist (fun s -> input_file_name :=s) usage_msg;
main (!input_file_name);
end
