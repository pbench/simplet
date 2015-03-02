(*
  TPLib: Tropical Polyhedra Library

  Copyright (C) 2009-2013 Xavier ALLAMIGEON (xavier.allamigeon at inria.fr)

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



(**
Numeric type
*)
module type T =
sig
  type t
  val zero : t
  val max : t -> t -> t
  val min : t -> t -> t
  val add : t -> t -> t
  val neg : t -> t
  val mul : t -> t -> t
  val div : t -> t -> t
  val pow : t -> int -> t
  val compare : t -> t -> int

  val of_int : int -> t
  val of_string : string -> t
  val to_string : t -> string
end

module Ocaml_int = struct
  type t = int

  let zero = 0
  let max = max
  let min = min
  let add x y = x+y
  let neg x = -x
  let mul x y = x*y
  let div x y = x/y

  let pow x n =
    let rec pow_aux x n =
      assert (n >= 0);
      if n = 0 then 1
      else
        let y = pow_aux x (n/2) in
        let z = y*y in
        if n mod 2 = 0 then
          z
        else
          x*z
    in
    if n >= 0 then
      pow_aux x n
    else
      invalid_arg "Ocaml_int.pow: the exponent should be non-negative."

  let compare = compare

  let of_int x = x
  let of_string = int_of_string
  let to_string = string_of_int
end

module Ocaml_float = struct
  type t = float

  let zero = 0.
  let max = max
  let min = min
  let add x y = x+.y
  let neg x = ~-. x
  let mul x y = x*.y
  let div x y = x/.y
  let pow x n = (x ** (float_of_int n))
  let compare = compare

  let of_int = float_of_int
  let of_string = float_of_string
  let to_string = string_of_float
end


module Ocaml_big_int = struct
  type t = Big_int.big_int

  let zero = Big_int.zero_big_int
  let max = Big_int.max_big_int
  let min = Big_int.min_big_int
  let add = Big_int.add_big_int
  let neg = Big_int.minus_big_int
  let mul = Big_int.mult_big_int
  let div = Big_int.div_big_int

  let pow x n =
    let rec pow_aux x n =
      assert (n >= 0);
      if n = 0 then Big_int.unit_big_int
      else
        let y = pow_aux x (n/2) in
        let z = mul y y in
        if n mod 2 = 0 then
          z
        else
          mul x z
    in
    if n >= 0 then
      pow_aux x n
    else
      invalid_arg "Ocaml_big_int.pow: the exponent should be non-negative."

  let compare = Big_int.compare_big_int
  let bits_of_string s =
    let res = ref zero in
    let n = String.length s in
    for i = 0 to (n-3) do
      res := Big_int.mult_int_big_int 2 !res;
      if s.[i+2] = '1' then
	res := Big_int.succ_big_int !res;
    done;
    !res

  let of_int = Big_int.big_int_of_int
  let of_string = Big_int.big_int_of_string
  let to_string = Big_int.string_of_big_int
end

module Ocaml_big_rat = struct
  type t = { num : Big_int.big_int;
	     den : Big_int.big_int }

  let zero = { num = Big_int.zero_big_int; den = Big_int.unit_big_int }

  let make num den =
    if Big_int.sign_big_int den = 0 then
      invalid_arg "Ocaml_big_rat: division by 0";
    if Big_int.sign_big_int num = 0 then
      zero
    else
      let x = Big_int.gcd_big_int num den in
      let res =
	{ num = Big_int.div_big_int num x;
	  den = Big_int.div_big_int den x }
      in
      if Big_int.sign_big_int res.den < 0 then
	{ num = Big_int.minus_big_int res.num;
	  den = Big_int.minus_big_int res.den }
      else
	res

  let compare x y =
    Big_int.compare_big_int
      (Big_int.mult_big_int x.num y.den)
      (Big_int.mult_big_int y.num x.den)

  let max x y = if compare x y >= 0 then x else y

  let min x y = if compare x y >= 0 then y else x

  let add x y =
    let num = Big_int.add_big_int
      (Big_int.mult_big_int x.num y.den)
      (Big_int.mult_big_int y.num x.den)
    in
    let den = Big_int.mult_big_int x.den y.den in
    make num den

  let neg x = { x with num = Big_int.minus_big_int x.num }

  let mul x y =
    let num = Big_int.mult_big_int x.num y.num
    and den = Big_int.mult_big_int x.den y.den in
    make num den

  let div x y =
    if Big_int.eq_big_int y.num Big_int.zero_big_int then
      raise Division_by_zero
    else
      let num = Big_int.mult_big_int x.num y.den
      and den = Big_int.mult_big_int x.den y.num in
      make num den

  let inv x = make x.den x.num

  let pow x n =
    let rec pow_aux x n =
      assert (n >= 0);
      if n = 0 then
        { num = Big_int.unit_big_int; den = Big_int.unit_big_int }
      else
        let y = pow_aux x (n/2) in
        let z = mul y y in
        if n mod 2 = 0 then
          z
        else
          mul x z
    in
    if n >= 0 then
      pow_aux x n
    else
      pow_aux (inv x) (-n)


  let of_int x =
    { num = Big_int.big_int_of_int x;
      den = Big_int.unit_big_int }

  let of_string s =
    try
      let i = String.index s '/' in
      make (Big_int.big_int_of_string (String.sub s 0 i))
	(Big_int.big_int_of_string (String.sub s (i+1) ((String.length s)-(i+1))))
    with Not_found -> { num = Big_int.big_int_of_string s;
			den = Big_int.unit_big_int }

  let to_string x =
    if Big_int.compare_big_int x.den Big_int.unit_big_int = 0 then
      Big_int.string_of_big_int x.num
    else
      (Big_int.string_of_big_int x.num)^"/"^(Big_int.string_of_big_int x.den)

end


(*let list_of_dynamic_modules = ["mlgmp_plugin"; "zarith_plugin"]*)

let list_of_modules =
  let l =
    [("ocaml_int", (module Ocaml_int: T));
     ("ocaml_float", (module Ocaml_float: T));    
     ("ocaml_big_int", (module Ocaml_big_int: T));
     ("ocaml_big_rat", (module Ocaml_big_rat: T))]
  in
  l
(*
  let add_dynamic_module name =
    try
      Findlib.init ();
      let path_name = (Findlib.package_directory Config.package_name)^"/" in
      Dynlink.loadfile (Dynlink.adapt_filename (path_name^name^".cma"))
    with Findlib.No_such_package _ -> ()
      | Dynlink.Error err -> ()
	(*print_endline (Dynlink.error_message err)*)
  in
  List.iter add_dynamic_module list_of_dynamic_modules;
  let l' =
    List.map (fun (s,m) ->
      let module M = (val m: Numeric_plugin.T) in
      (s, (module M: T)))
      (Numeric_plugin.to_list ())
  in
  l@l'
*)

(*
let default_integer_module = "ocaml_int"
let default_exact_rational_module = "ocaml_big_rat"
*)


let get s =
  try
    List.assoc s list_of_modules
  with Not_found -> invalid_arg (s^": unknown type of numerical data")

let get_name_of_modules () =
  List.map fst list_of_modules
