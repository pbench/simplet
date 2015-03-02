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

(** Additive (commutative) group


*)
  module type T = 
  sig
    type t
    val zero : t
    val add : t->t->t
    val sum : t list -> t
    val substract : t->t->t
    val neg : t-> t
    val to_string : t -> string
    (*val from_string : string -> t 
    val regexp_string : string*)
    val operation_string :  string
  end

(** Ordered group
*)
  module type Ordered = 
  sig
    include T
    val compare : t-> t-> int
    val max : t-> t -> t
  end


  module Make ( Numeric : Numeric.T) : Ordered with type t = Numeric.t =
  struct
    type t = Numeric.t
    let zero = Numeric.zero
    let add x y = Numeric.add x  y
    let neg x = Numeric.neg x
    let to_string x = Numeric.to_string x
    let compare  = Numeric.compare

    let sum list = 
      match list with
        | [] -> assert false
        |  x::tail -> List.fold_left add x tail
    
    let substract x y =add x (neg y)
   
    let operation_string ="+"
     
    let max x y =
      match compare x y with
      | 1 | 0 -> x 
      | -1 -> y
      | _ -> assert false
  end

  module Int : Ordered with type t = int=
  struct
    type t = int
    let zero = 0
    let add x y = x + y
    let sum list = 
      match list with
        | [] -> assert false
        |  x::tail -> List.fold_left add x tail
    let substract x y = x - y
    let neg x = -x
    let to_string x = string_of_int x
    let from_string s = int_of_string s 
    let regexp_string = "-?[0-9]+"
    let operation_string ="+"
    let compare  = compare 
    let max = max

  end


  module ReverseOrder  (Group : Ordered) = 
  struct
    type t = Group.t
    let zero = Group.zero
    let add = Group.add
    let sum = Group.sum
    let substract = Group.substract
    let neg = Group.neg
    let to_string = Group.to_string
    (*let from_string = Group.from_string 
    let regexp_string = Group.regexp_string*)
    let operation_string = Group.operation_string
    let compare x y  = 
      match Group.compare x y with
        | 1 -> -1
        | 0 -> 0
        | -1 -> 1
        |_ -> assert false
    let max x y = match compare x y with
      | 1 | 0 -> x 
      | -1 -> y
      | _ -> assert false

  end


  module type CartesianProduct =
  sig
    include Ordered
    module G : Ordered
    module H : Ordered
    val first : t ->  G.t
    val second : t -> H.t
    val from_entries : G.t -> H.t -> t
  end

  module MakeCartesianProduct (G : Ordered) (H : Ordered) : (CartesianProduct with module G = G with module H = H)=
  struct
    type t = G.t * H.t
    let zero = (G.zero, H.zero)
    let add (x_g, x_h) (y_g, y_h) = (G.add x_g y_g, H.add x_h y_h)
    let sum list = 
      match list with
        | [] -> assert false
        | x::tail -> List.fold_left add x tail
    let substract  (x_g, x_h) (y_g, y_h) = (G.substract x_g y_g, H.substract x_h y_h)
    let neg (x_g, x_h) = (G.neg x_g, H.neg x_h)
    let to_string (x_g, x_h) = Format.sprintf "[|%s; %s|]" (G.to_string x_g) (H.to_string x_h)
    let operation_string  = 
      match (String.compare G.operation_string G.operation_string) with
        | 0 -> G.operation_string
        | _ -> Format.sprintf "(%s, %s)" G.operation_string H.operation_string 
    let compare (x_g, x_h) (y_g, y_h) = 
      match (G.compare x_g y_g) with
        | 1 -> 1
        | -1 -> -1
        | 0 -> H.compare x_h y_h
        | _ -> assert false
    let max x y = 
      match compare x y with
        | 1 | 0 -> x
        | _ -> y

    module G = G
    module H = H
    let first (g,h) = g
    let second (g,h) = h
    let from_entries g h = (g,h)

    (*let regexp_string = "\\[|" ^ G.regexp_string ^ ";"^ H.regexp_string ^ "|\\]" 
    
    let from_string s =
      (*let whitespaces="[ \\012\\n\\r\\t]" in*)
      let blank_regexp = Str.regexp "[ \\t]+" in
      let s_wo_blanks = Str.global_replace blank_regexp "" s in
      let matched = Str.string_match (Str.regexp regexp_string) s_wo_blanks 0 in
      if ( matched = false) then
        invalid_arg("Error, CartesianPower.from_string: bad input string, must be of the form [| a ; b |], where:
- a is of the form "^G.regexp_string^"
- b is of the form "^H.regexp_string^"\n");
      let g_matched = Str.string_match (Str.regexp G.regexp_string) s_wo_blanks 2 in
      assert g_matched;
      let g_string = Str.matched_string s_wo_blanks in

      let g_end = Str.match_end() in
      let h_matched = Str.string_match (Str.regexp H.regexp_string) s_wo_blanks (g_end+1) in
      assert h_matched;
      let h_string = Str.matched_string s_wo_blanks in
      from_entries (G.from_string g_string) (H.from_string h_string)
        
    *)



  end


  module type CartesianTriple =
  sig
    include Ordered
    module F : Ordered
    module G : Ordered
    module H : Ordered
    val first : t ->  F.t
    val second : t -> G.t
    val third : t -> H.t
    val from_entries : F.t -> G.t -> H.t -> t
  end

  module MakeCartesianTriple (F : Ordered) (G : Ordered) (H : Ordered) : (CartesianTriple with module F=F with module G = G with module H = H)=
  struct
    type t = F.t * G.t * H.t
    let zero = (F.zero,G.zero, H.zero)
    let add (x_f, x_g, x_h) (y_f, y_g, y_h) = (F.add x_f y_f, G.add x_g y_g, H.add x_h y_h)
    let sum list = 
      match list with
        | [] -> assert false
        | x::tail -> List.fold_left add x tail
    let substract  (x_f, x_g, x_h) (y_f, y_g, y_h) = (F.substract x_f y_f, G.substract x_g y_g, H.substract x_h y_h)
    let neg (x_f, x_g, x_h) = (F.neg x_f, G.neg x_g, H.neg x_h)
    let to_string (x_f, x_g, x_h) = Format.sprintf "[|%s; %s; %s|]" (F.to_string x_f) (G.to_string x_g) (H.to_string x_h)
    let operation_string  = "+"
    let compare (x_f, x_g, x_h) (y_f, y_g, y_h) = 
      match (F.compare x_f y_f, G.compare x_g y_g, H.compare x_h y_h) with
        | (1,_,_) -> 1
        | (-1,_,_) -> -1
        | (0,1,_) -> 1
        | (0,-1,_) -> -1
        | (0,0,1) -> 1
        | (0,0,-1) -> -1
        | (0,0,0) -> 0
        | _ -> assert false
    let max x y = 
      match compare x y with
        | 1 | 0 -> x
        | _ -> y

    module F = F
    module G = G
    module H = H
    let first (f,g,h) = f
    let second (f,g,h) = g
    let third (f,g,h) = h
    let from_entries f g h = (f,g,h)

    (*let regexp_string = "\\[|" ^ G.regexp_string ^ ";"^ H.regexp_string ^ "|\\]" 
    
    let from_string s =
      (*let whitespaces="[ \\012\\n\\r\\t]" in*)
      let blank_regexp = Str.regexp "[ \\t]+" in
      let s_wo_blanks = Str.global_replace blank_regexp "" s in
      let matched = Str.string_match (Str.regexp regexp_string) s_wo_blanks 0 in
      if ( matched = false) then
        invalid_arg("Error, CartesianPower.from_string: bad input string, must be of the form [| a ; b |], where:
- a is of the form "^G.regexp_string^"
- b is of the form "^H.regexp_string^"\n");
      let g_matched = Str.string_match (Str.regexp G.regexp_string) s_wo_blanks 2 in
      assert g_matched;
      let g_string = Str.matched_string s_wo_blanks in

      let g_end = Str.match_end() in
      let h_matched = Str.string_match (Str.regexp H.regexp_string) s_wo_blanks (g_end+1) in
      assert h_matched;
      let h_string = Str.matched_string s_wo_blanks in
      from_entries (G.from_string g_string) (H.from_string h_string)
        
    *)

end

(*
  module type CartesianPower =
  sig
    include Ordered
    module G : Ordered
    val dim : int
    val get : t -> int -> G.t
    val from_array : G.t array -> t 
    val from_list : (int * G.t) list -> t      
  end

  module MakeCartesianPower (Group : Ordered) (Dim : sig val dim : int end) : (CartesianPower with module G  = Group) = 
  struct
          module G = Group

          type t = G.t array
          let dim = Dim.dim

          let zero = Array.make dim G.zero

          let add x y = 
            let result = Array.make dim G.zero in
            for i = 0 to (dim-1) do
              let result_i = G.add x.(i) y.(i) in
              Array.set result i result_i
            done;
            result
                
          let sum list = 
            match list with
              | [] -> assert false
              | x::tail -> List.fold_left add x tail

          let substract x y = 
            let result = Array.make dim G.zero in
            for i = 0 to (dim-1) do
              let result_i = G.substract x.(i) y.(i) in
              Array.set result i result_i
            done;
            result

          let neg x = 
            let result = Array.make dim G.zero in
            for i = 0 to (dim-1) do
              let result_i = G.neg x.(i)  in
              Array.set result i result_i
            done;
            result

          let to_string x = 
            let rec printer string i = 
              if i >= dim then string
              else 
                let new_string = 
                  if i = 0 then Format.sprintf "(%s"  (G.to_string x.(i))
                  else  if i = dim-1 then Format.sprintf "%s, %s )" string (G.to_string x.(i))
                  else Format.sprintf "%s, %s" string (G.to_string x.(i))
                in printer new_string (i+1)
            in
            printer "" 0
         

          let operation_string = G.operation_string

          let compare x y =
            let rec comparator i =
              if i>=dim then 0
              else 
                match (G.compare x.(i) y.(i)) with
                  | 1 -> 1
                  | -1 -> -1
                  | 0 -> comparator (i+1)
                  | _ -> assert false
            in
            comparator 0
               

          let max x y = 
            match compare x y with
              | 1 | 0 -> x
              | _ -> y
            


          let get x i = 
            if (i < 0 || i >= dim ) then
                invalid_arg("Error, CartesianPower.get: index out of bounds")
                else
                Array.get x i

          let from_array input = 
            assert (Array.length input = dim);
            input

          let from_list list =
            let x = Array.make dim G.zero in
            List.iter (fun (i, x_i) ->
              let old_x_i = get x i in
              if ((G.compare old_x_i G.zero) <>0) then
                invalid_arg(Format.sprintf "Error, CartesianPower.from_list: entry indexed by %i defined twice" i)
              else
                Array.set x i x_i
            ) list;
            x
            

          let regexp_string = "\\[\\|\\(G.regexp_string\\)+\\|\\]"
    end
    *)

(** Group of sequences of elements of {b G}, ordered lexicographically, where {b G} is an ordered group

encoded as lists, number of elements limited by memory
*)
  module type CartesianPowerSparse =
  sig
    include Ordered
    module G : Ordered

    val get : t -> int -> G.t

    val from_list : (int * G.t) list -> t      
  end

  module MakeCartesianPowerSparse  (Group : Ordered) = 
  struct
          module G = Group
            
            
          type t = (int * G.t) list (*sorted by increasing index order*)


          let zero = []

          let add x y = 
            let rec aux x y result = 
              match x with
                | [] -> List.rev_append result y
                | (i_x, entry_x):: tail_x ->
                    begin
                      match y with
                        | [] ->  List.rev_append result  x
                        | (i_y, entry_y) :: tail_y ->
                            begin
                            match (compare i_x i_y) with
                              | 1 -> let new_result = (i_y, entry_y) :: result in
                                     aux x tail_y new_result
                              | -1 -> let new_result = (i_x, entry_x) :: result in
                                      aux tail_x y new_result
                              | 0 -> let sum = G.add entry_x entry_y in
                                     let new_result = match G.compare sum G.zero with
                                       | 0 -> result
                                       | _ -> (i_x, sum) :: result
                                     in
                                     aux tail_x tail_y new_result
                              | _ -> assert false
                            end
                    end
            in
            aux x y []

           
                
          let sum list = 
            match list with
              | [] -> assert false
              | x::tail -> List.fold_left add x tail

          let neg x = 
            List.map (fun (i, entry) -> (i, G.neg entry)) x
 

          let substract x y = 
            let neg_y = neg y in
            add x neg_y

          let to_string x = 
            let rec printer x string = 
              match x with 
                | (i, entry)::tail -> let s = Format.sprintf "%s; (%i,%s)" string i (G.to_string entry) in
                                      printer tail s
                | [] -> Format.sprintf "%s]" string
            in
            match x with
              | [] -> "[]"
              | (i, entry)::tail -> 
                  let s = Format.sprintf "[(%i,%s)" i (G.to_string entry)in
                  printer tail s
         

          let operation_string = G.operation_string

          let compare x y = 
            let rec comparator x y =
              match x with
                | [] -> 
                    begin
                      match y with
                        | [] -> 0
                        | (i_y, entry_y) :: _ -> 
                            match G.compare entry_y G.zero with
                              | 1 -> -1
                              | -1 -> 1
                              | _ -> assert false
                    end
                | (i_x, entry_x) :: tail_x ->
                    begin
                      match y with
                        | [] -> 
                            begin
                              match G.compare entry_x G.zero with
                                | 1 -> 1
                                | -1 -> -1
                                | _ -> assert false
                            end
                        | (i_y, entry_y) :: tail_y -> 
                            begin
                              match compare i_x i_y with
                                | 1 -> 
                                    begin
                                      match G.compare entry_y G.zero with
                                        | 1 -> -1
                                        | -1 -> 1
                                        | _ -> assert false
                                    end
                                | -1 -> 
                                    begin
                                      match G.compare entry_x G.zero with
                                        | 1 -> 1
                                        | -1 -> -1
                                        | _ -> assert false
                                    end
                                | 0 -> 
                                    begin
                                      match G.compare entry_x entry_y with
                                        | 1 -> 1
                                        | -1 -> -1
                                        | 0 -> comparator tail_x tail_y
                                        | _ -> assert false
                                    end
                                | _ -> assert false
                            (*
                              match ( (compare i_x i_y), (G.compare entry_x G.zero),(G.compare entry_y G.zero), (G.compare entry_x entry_y)) with

                                | (1,_,1,_) -> -1
                                | (1,_,-1,_) -> 1
                                | (1,_,_,_) -> assert false
                                | (-1, 1, _,_) -> 1
                                | (-1, -1,_, _) -> -1
                                | (-1, _, _, _) -> assert false
                                | (0,_,_,1) -> 1
                                | (0,_,_,-1) -> -1
                                | (0,_,_,0) -> compare tail_x tail_y
                                | (0,_,_,_) -> assert false
*)
                            end
                    end
            in
            comparator x y 

          let max x y = 
            match compare x y with
              | 1 | 0 -> x
              | _ -> y
            

                  
          let get x index = 
            if (index < 0 )  then
                invalid_arg("Error, CartesianPower.get: index out of bounds")
            else
              let rec finder list = 
                match list with
                  | [] -> G.zero
                  | (i, x_i)::_ when (i = index) -> x_i
                  | _::tail -> finder tail
              in
              finder x
                      
                        


          let from_list list =
            let sort_f (i, x_i) (j, x_j) = Int.compare i j in
            let sorted_list = List.fast_sort sort_f list in
            let rec checker previous_i list =
              match list with 
                | [] -> ()
                | (i,x_i) :: tail ->           
                    begin
                      if (i < 0 ) then
                        invalid_arg("Error, CartesianPowerSparse.from_list: index out of bounds");
                      if( i= previous_i) then
                        invalid_arg(Format.sprintf "Error, CartesianPowerSparse.from_list: entry indexed by %i defined twice" i);
                      
(*                      if ( ( G.compare x_i G.zero) = 0 ) then                    
                          (Format.printf "Warning, CartesianPowerSparse.from_list: entry indexed by %i is defined to be zero\n" i);                   *)
                      checker i tail
                    end
            in
            checker (-1) sorted_list;
            List.filter (fun (i, x_i) -> (G.compare x_i G.zero) <> 0 ) sorted_list

              
      (*    let regexp_string = "\\[([0-9]+,"^G.regexp_string^")\\(;([0-9]+,"^G.regexp_string^")\\)+\\]"
            
          let from_string s = 
            let blank_regexp = Str.regexp "[ \\t]+" in
            let s_wo_blanks = Str.global_replace blank_regexp "" s in

           (* Format.printf "%s\n" s_wo_blanks;*)
            let entry_regexp = Str.regexp ("(\\([0-9]+\\),\\("^G.regexp_string^"\\))") in
           
            let string_ok = Str.string_match (Str.regexp regexp_string) s_wo_blanks 0 in
            if (string_ok = false) then
               invalid_arg("Error, CartesianPowerSparse.from_string: bad input string, must be of the form
 [ (index,value); (index_1, value_1); ... ; (index_n, value_n) ]
where
-index_i are nonnegative integers
-value_i are of the form "^G.regexp_string^"
 ");

            let rec parse list string_position =
              let matched = Str.string_match entry_regexp s_wo_blanks (string_position+1) in
              if (matched = false) then list
              else
                let entry_string = Str.matched_group 2 s_wo_blanks in
                let entry_index_string = Str.matched_group 1 s_wo_blanks in
                let entry = G.from_string entry_string in
                let index = int_of_string entry_index_string in
                let next_pos = Str.match_end() in
                let new_list = (index, entry)::list in
                parse new_list next_pos
            in
            let list = parse [] 0 in
            from_list list


              
    let lexer = make_lexer ["[";"]";"(";")";","]
    let from_string1 s =
      let rec parse_entry = parser
        | [< 'Kwd "("; 'Int index;  'Kwd ","; v = G.from_string ; 'Kwd ")" >] -> (index,v)
      and parse_list old_list = parser
        | [< e = parse_entry; tail >] -> 
            (parser
              | [< 'Kwd ","; tail_ >] -> parse_list (e::old_list) tail_
              | [< >] -> e::old_list
            ) tail
        | [< >] -> old_list
      and parse_all = parser
        | [< 'Kwd "["; list = (parse_list []) ; 'Kwd "]" >] -> list
      in
      parse_all (lexer (Stream.of_string s) )

      *)
         
    end
