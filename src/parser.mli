module Make
          (Numeric : sig
  type t
  val zero : t
  val add : t -> t -> t
  val neg : t -> t
  val mul : t -> t -> t
  val div : t -> t -> t
  val of_string : string -> t
  val to_string : t -> string
end)
: sig

  exception Error
  
  
  val main: (Lexing.lexbuf -> Tokens.token) -> Lexing.lexbuf -> ( (LinearProg.col_index_t * LinearProg.sign_t * Numeric.t) list *
  ((LinearProg.col_index_t * LinearProg.sign_t * Numeric.t) list) list *
  Numeric.t list)

end
