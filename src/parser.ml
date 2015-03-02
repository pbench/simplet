module Make

# 17 "_parser.mly"
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
# 15 "parser.ml"

= struct

  exception Error
  
  type _menhir_env = {
    _menhir_lexer: Lexing.lexbuf -> Tokens.token;
    _menhir_lexbuf: Lexing.lexbuf;
    mutable _menhir_token: Tokens.token;
    mutable _menhir_startp: Lexing.position;
    mutable _menhir_endp: Lexing.position;
    mutable _menhir_shifted: int
  }
  
  and _menhir_state = 
    | MenhirState58
    | MenhirState54
    | MenhirState50
    | MenhirState44
    | MenhirState41
    | MenhirState38
    | MenhirState35
    | MenhirState29
    | MenhirState26
    | MenhirState22
    | MenhirState19
    | MenhirState15
    | MenhirState13
    | MenhirState11
    | MenhirState8
    | MenhirState6
    | MenhirState5
    | MenhirState3
    | MenhirState1
  
  
# 58 "_parser.mly"
  

  open LinearProg

  


# 60 "parser.ml"
  let _eRR =
    Error
  
  let rec _menhir_goto_nonempty_list_ineq_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_nonempty_list_ineq_ -> 'ttv_return =
    fun _menhir_env _menhir_stack _menhir_s _v ->
      match _menhir_s with
      | MenhirState38 ->
          let (_menhir_env : _menhir_env) = _menhir_env in
          let (_menhir_stack : 'freshtv325) = Obj.magic _menhir_stack in
          let (_menhir_s : _menhir_state) = _menhir_s in
          let (_v : 'tv_nonempty_list_ineq_) = _v in
          ((let (_menhir_env : _menhir_env) = _menhir_env in
          let (_menhir_stack : 'freshtv323) = Obj.magic _menhir_stack in
          let (_menhir_s : _menhir_state) = _menhir_s in
          let (l : 'tv_nonempty_list_ineq_) = _v in
          ((let _v : 'tv_ineqs = 
# 124 "_parser.mly"
                          (l)
# 79 "parser.ml"
           in
          let (_menhir_env : _menhir_env) = _menhir_env in
          let (_menhir_stack : 'freshtv321) = _menhir_stack in
          let (_menhir_s : _menhir_state) = _menhir_s in
          let (_v : 'tv_ineqs) = _v in
          ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
          let (_menhir_env : _menhir_env) = _menhir_env in
          let (_menhir_stack : ('freshtv319 * 'tv_objective) * _menhir_state * 'tv_ineqs) = Obj.magic _menhir_stack in
          ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
          let _tok = _menhir_env._menhir_token in
          let (_menhir_env : _menhir_env) = _menhir_env in
          let (_menhir_stack : ('freshtv317 * 'tv_objective) * _menhir_state * 'tv_ineqs) = _menhir_stack in
          let (_tok : Tokens.token) = _tok in
          ((match _tok with
          | Tokens.BASIC_POINT ->
              let (_menhir_env : _menhir_env) = _menhir_env in
              let (_menhir_stack : 'freshtv309) = Obj.magic _menhir_stack in
              ((let _tok = _menhir_discard _menhir_env in
              let (_menhir_env : _menhir_env) = _menhir_env in
              let (_menhir_stack : 'freshtv307) = _menhir_stack in
              let (_tok : Tokens.token) = _tok in
              ((match _tok with
              | Tokens.EQ ->
                  let (_menhir_env : _menhir_env) = _menhir_env in
                  let (_menhir_stack : 'freshtv303) = Obj.magic _menhir_stack in
                  ((let _tok = _menhir_discard _menhir_env in
                  let (_menhir_env : _menhir_env) = _menhir_env in
                  let (_menhir_stack : 'freshtv301) = _menhir_stack in
                  let (_tok : Tokens.token) = _tok in
                  ((match _tok with
                  | Tokens.LPAREN ->
                      _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState50
                  | Tokens.MINUS ->
                      _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState50
                  | Tokens.NUM _v ->
                      _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _v
                  | _ ->
                      assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                      _menhir_env._menhir_shifted <- (-1);
                      _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState50) : 'freshtv302)) : 'freshtv304)
              | _ ->
                  assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                  _menhir_env._menhir_shifted <- (-1);
                  let (_menhir_env : _menhir_env) = _menhir_env in
                  let (_menhir_stack : 'freshtv305) = Obj.magic _menhir_stack in
                  (raise _eRR : 'freshtv306)) : 'freshtv308)) : 'freshtv310)
          | Tokens.EOF ->
              let (_menhir_env : _menhir_env) = _menhir_env in
              let (_menhir_stack : ('freshtv313 * 'tv_objective) * _menhir_state * 'tv_ineqs) = Obj.magic _menhir_stack in
              ((let (_menhir_env : _menhir_env) = _menhir_env in
              let (_menhir_stack : ('freshtv311 * 'tv_objective) * _menhir_state * 'tv_ineqs) = Obj.magic _menhir_stack in
              ((let ((_menhir_stack, o), _, cl) = _menhir_stack in
              let _v : (
# 46 "_parser.mly"
       ( (LinearProg.col_index_t * LinearProg.sign_t * Numeric.t) list *
  ((LinearProg.col_index_t * LinearProg.sign_t * Numeric.t) list) list *
  Numeric.t list)
# 137 "parser.ml"
              ) = 
# 111 "_parser.mly"
                               ( (o, cl, []) )
# 141 "parser.ml"
               in
              _menhir_goto_main _menhir_env _menhir_stack _v) : 'freshtv312)) : 'freshtv314)
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              let (_menhir_env : _menhir_env) = _menhir_env in
              let (_menhir_stack : ('freshtv315 * 'tv_objective) * _menhir_state * 'tv_ineqs) = Obj.magic _menhir_stack in
              ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv316)) : 'freshtv318)) : 'freshtv320)) : 'freshtv322)) : 'freshtv324)) : 'freshtv326)
      | MenhirState58 ->
          let (_menhir_env : _menhir_env) = _menhir_env in
          let (_menhir_stack : 'freshtv329 * _menhir_state * 'tv_ineq) = Obj.magic _menhir_stack in
          let (_menhir_s : _menhir_state) = _menhir_s in
          let (_v : 'tv_nonempty_list_ineq_) = _v in
          ((let (_menhir_env : _menhir_env) = _menhir_env in
          let (_menhir_stack : 'freshtv327 * _menhir_state * 'tv_ineq) = Obj.magic _menhir_stack in
          let (_ : _menhir_state) = _menhir_s in
          let (xs : 'tv_nonempty_list_ineq_) = _v in
          ((let (_menhir_stack, _menhir_s, x) = _menhir_stack in
          let _v : 'tv_nonempty_list_ineq_ = 
# 126 "/usr/share/menhir/standard.mly"
    ( x :: xs )
# 164 "parser.ml"
           in
          _menhir_goto_nonempty_list_ineq_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv328)) : 'freshtv330)
      | _ ->
          _menhir_fail ()
  
  and _menhir_goto_ineq : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_ineq -> 'ttv_return =
    fun _menhir_env _menhir_stack _menhir_s _v ->
      let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
      let (_menhir_env : _menhir_env) = _menhir_env in
      let (_menhir_stack : 'freshtv299 * _menhir_state * 'tv_ineq) = Obj.magic _menhir_stack in
      ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
      let _tok = _menhir_env._menhir_token in
      let (_menhir_env : _menhir_env) = _menhir_env in
      let (_menhir_stack : 'freshtv297 * _menhir_state * 'tv_ineq) = _menhir_stack in
      let (_tok : Tokens.token) = _tok in
      ((match _tok with
      | Tokens.LPAREN ->
          _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState58
      | Tokens.MAX ->
          _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState58
      | Tokens.MINUS ->
          _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState58
      | Tokens.NUM _v ->
          _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _v
      | Tokens.VAR _v ->
          _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _v
      | Tokens.BASIC_POINT | Tokens.EOF ->
          let (_menhir_env : _menhir_env) = _menhir_env in
          let (_menhir_stack : 'freshtv295 * _menhir_state * 'tv_ineq) = Obj.magic _menhir_stack in
          ((let (_menhir_stack, _menhir_s, x) = _menhir_stack in
          let _v : 'tv_nonempty_list_ineq_ = 
# 124 "/usr/share/menhir/standard.mly"
    ( [ x ] )
# 198 "parser.ml"
           in
          _menhir_goto_nonempty_list_ineq_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv296)
      | _ ->
          assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
          _menhir_env._menhir_shifted <- (-1);
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState58) : 'freshtv298)) : 'freshtv300)
  
  and _menhir_goto_objective : _menhir_env -> 'ttv_tail -> 'tv_objective -> 'ttv_return =
    fun _menhir_env _menhir_stack _v ->
      let _menhir_stack = (_menhir_stack, _v) in
      let (_menhir_env : _menhir_env) = _menhir_env in
      let (_menhir_stack : 'freshtv293 * 'tv_objective) = Obj.magic _menhir_stack in
      ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
      let _tok = _menhir_env._menhir_token in
      let (_menhir_env : _menhir_env) = _menhir_env in
      let (_menhir_stack : 'freshtv291 * 'tv_objective) = _menhir_stack in
      let (_tok : Tokens.token) = _tok in
      ((match _tok with
      | Tokens.LPAREN ->
          _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState38
      | Tokens.MAX ->
          _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState38
      | Tokens.MINUS ->
          _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState38
      | Tokens.NUM _v ->
          _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v
      | Tokens.VAR _v ->
          _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v
      | _ ->
          assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
          _menhir_env._menhir_shifted <- (-1);
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState38) : 'freshtv292)) : 'freshtv294)
  
  and _menhir_goto_main : _menhir_env -> 'ttv_tail -> (
# 46 "_parser.mly"
       ( (LinearProg.col_index_t * LinearProg.sign_t * Numeric.t) list *
  ((LinearProg.col_index_t * LinearProg.sign_t * Numeric.t) list) list *
  Numeric.t list)
# 237 "parser.ml"
  ) -> 'ttv_return =
    fun _menhir_env _menhir_stack _v ->
      let (_menhir_env : _menhir_env) = _menhir_env in
      let (_menhir_stack : 'freshtv289) = Obj.magic _menhir_stack in
      let (_v : (
# 46 "_parser.mly"
       ( (LinearProg.col_index_t * LinearProg.sign_t * Numeric.t) list *
  ((LinearProg.col_index_t * LinearProg.sign_t * Numeric.t) list) list *
  Numeric.t list)
# 247 "parser.ml"
      )) = _v in
      ((let (_menhir_env : _menhir_env) = _menhir_env in
      let (_menhir_stack : 'freshtv287) = Obj.magic _menhir_stack in
      let (_1 : (
# 46 "_parser.mly"
       ( (LinearProg.col_index_t * LinearProg.sign_t * Numeric.t) list *
  ((LinearProg.col_index_t * LinearProg.sign_t * Numeric.t) list) list *
  Numeric.t list)
# 256 "parser.ml"
      )) = _v in
      (Obj.magic _1 : 'freshtv288)) : 'freshtv290)
  
  and _menhir_fail : unit -> 'a =
    fun () ->
      Printf.fprintf Pervasives.stderr "Internal failure -- please contact the parser generator's developers.\n%!";
      assert false
  
  and _menhir_goto_linear_form : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_linear_form -> 'ttv_return =
    fun _menhir_env _menhir_stack _menhir_s _v ->
      let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
      match _menhir_s with
      | MenhirState1 ->
          let (_menhir_env : _menhir_env) = _menhir_env in
          let (_menhir_stack : ('freshtv241) * _menhir_state * 'tv_linear_form) = Obj.magic _menhir_stack in
          ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
          let _tok = _menhir_env._menhir_token in
          let (_menhir_env : _menhir_env) = _menhir_env in
          let (_menhir_stack : ('freshtv239) * _menhir_state * 'tv_linear_form) = _menhir_stack in
          let (_tok : Tokens.token) = _tok in
          ((match _tok with
          | Tokens.SEMICOLON ->
              let (_menhir_env : _menhir_env) = _menhir_env in
              let (_menhir_stack : ('freshtv235) * _menhir_state * 'tv_linear_form) = Obj.magic _menhir_stack in
              ((let _ = _menhir_discard _menhir_env in
              let (_menhir_env : _menhir_env) = _menhir_env in
              let (_menhir_stack : ('freshtv233) * _menhir_state * 'tv_linear_form) = Obj.magic _menhir_stack in
              ((let (_menhir_stack, _, l) = _menhir_stack in
              let _v : 'tv_objective = 
# 119 "_parser.mly"
                                    (
  List.rev_map (fun (j, v) -> (j, Pos, v) ) l
)
# 290 "parser.ml"
               in
              _menhir_goto_objective _menhir_env _menhir_stack _v) : 'freshtv234)) : 'freshtv236)
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              let (_menhir_env : _menhir_env) = _menhir_env in
              let (_menhir_stack : ('freshtv237) * _menhir_state * 'tv_linear_form) = Obj.magic _menhir_stack in
              ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv238)) : 'freshtv240)) : 'freshtv242)
      | MenhirState35 ->
          let (_menhir_env : _menhir_env) = _menhir_env in
          let (_menhir_stack : ('freshtv251) * _menhir_state * 'tv_linear_form) = Obj.magic _menhir_stack in
          ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
          let _tok = _menhir_env._menhir_token in
          let (_menhir_env : _menhir_env) = _menhir_env in
          let (_menhir_stack : ('freshtv249) * _menhir_state * 'tv_linear_form) = _menhir_stack in
          let (_tok : Tokens.token) = _tok in
          ((match _tok with
          | Tokens.SEMICOLON ->
              let (_menhir_env : _menhir_env) = _menhir_env in
              let (_menhir_stack : ('freshtv245) * _menhir_state * 'tv_linear_form) = Obj.magic _menhir_stack in
              ((let _ = _menhir_discard _menhir_env in
              let (_menhir_env : _menhir_env) = _menhir_env in
              let (_menhir_stack : ('freshtv243) * _menhir_state * 'tv_linear_form) = Obj.magic _menhir_stack in
              ((let (_menhir_stack, _, l) = _menhir_stack in
              let _v : 'tv_objective = 
# 116 "_parser.mly"
                                     (
  List.rev_map (fun (j, v) -> (j, Neg, v) ) l
)
# 321 "parser.ml"
               in
              _menhir_goto_objective _menhir_env _menhir_stack _v) : 'freshtv244)) : 'freshtv246)
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              let (_menhir_env : _menhir_env) = _menhir_env in
              let (_menhir_stack : ('freshtv247) * _menhir_state * 'tv_linear_form) = Obj.magic _menhir_stack in
              ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv248)) : 'freshtv250)) : 'freshtv252)
      | MenhirState58 | MenhirState38 ->
          let (_menhir_env : _menhir_env) = _menhir_env in
          let (_menhir_stack : 'freshtv265 * _menhir_state * 'tv_linear_form) = Obj.magic _menhir_stack in
          ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
          let _tok = _menhir_env._menhir_token in
          let (_menhir_env : _menhir_env) = _menhir_env in
          let (_menhir_stack : 'freshtv263 * _menhir_state * 'tv_linear_form) = _menhir_stack in
          let (_tok : Tokens.token) = _tok in
          ((match _tok with
          | Tokens.GEQ ->
              let (_menhir_env : _menhir_env) = _menhir_env in
              let (_menhir_stack : 'freshtv255 * _menhir_state * 'tv_linear_form) = Obj.magic _menhir_stack in
              ((let _tok = _menhir_discard _menhir_env in
              let (_menhir_env : _menhir_env) = _menhir_env in
              let (_menhir_stack : 'freshtv253 * _menhir_state * 'tv_linear_form) = _menhir_stack in
              let (_tok : Tokens.token) = _tok in
              ((match _tok with
              | Tokens.LPAREN ->
                  _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState44
              | Tokens.MAX ->
                  _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState44
              | Tokens.MINUS ->
                  _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState44
              | Tokens.NUM _v ->
                  _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v
              | Tokens.VAR _v ->
                  _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v
              | _ ->
                  assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                  _menhir_env._menhir_shifted <- (-1);
                  _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState44) : 'freshtv254)) : 'freshtv256)
          | Tokens.LEQ ->
              let (_menhir_env : _menhir_env) = _menhir_env in
              let (_menhir_stack : 'freshtv259 * _menhir_state * 'tv_linear_form) = Obj.magic _menhir_stack in
              ((let _tok = _menhir_discard _menhir_env in
              let (_menhir_env : _menhir_env) = _menhir_env in
              let (_menhir_stack : 'freshtv257 * _menhir_state * 'tv_linear_form) = _menhir_stack in
              let (_tok : Tokens.token) = _tok in
              ((match _tok with
              | Tokens.LPAREN ->
                  _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState41
              | Tokens.MAX ->
                  _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState41
              | Tokens.MINUS ->
                  _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState41
              | Tokens.NUM _v ->
                  _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _v
              | Tokens.VAR _v ->
                  _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _v
              | _ ->
                  assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                  _menhir_env._menhir_shifted <- (-1);
                  _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState41) : 'freshtv258)) : 'freshtv260)
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              let (_menhir_env : _menhir_env) = _menhir_env in
              let (_menhir_stack : 'freshtv261 * _menhir_state * 'tv_linear_form) = Obj.magic _menhir_stack in
              ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv262)) : 'freshtv264)) : 'freshtv266)
      | MenhirState41 ->
          let (_menhir_env : _menhir_env) = _menhir_env in
          let (_menhir_stack : ('freshtv275 * _menhir_state * 'tv_linear_form) * _menhir_state * 'tv_linear_form) = Obj.magic _menhir_stack in
          ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
          let _tok = _menhir_env._menhir_token in
          let (_menhir_env : _menhir_env) = _menhir_env in
          let (_menhir_stack : ('freshtv273 * _menhir_state * 'tv_linear_form) * _menhir_state * 'tv_linear_form) = _menhir_stack in
          let (_tok : Tokens.token) = _tok in
          ((match _tok with
          | Tokens.SEMICOLON ->
              let (_menhir_env : _menhir_env) = _menhir_env in
              let (_menhir_stack : ('freshtv269 * _menhir_state * 'tv_linear_form) * _menhir_state * 'tv_linear_form) = Obj.magic _menhir_stack in
              ((let _ = _menhir_discard _menhir_env in
              let (_menhir_env : _menhir_env) = _menhir_env in
              let (_menhir_stack : ('freshtv267 * _menhir_state * 'tv_linear_form) * _menhir_state * 'tv_linear_form) = Obj.magic _menhir_stack in
              ((let ((_menhir_stack, _menhir_s, l1), _, l2) = _menhir_stack in
              let _v : 'tv_ineq = 
# 127 "_parser.mly"
                                                  (
  let lpos = List.rev_map (fun (j, v) -> (j, Pos, v) ) l2 in
  let lneg = List.rev_map (fun (j,v) -> (j, Neg, v) ) l1 in
  List.rev_append lpos lneg
)
# 414 "parser.ml"
               in
              _menhir_goto_ineq _menhir_env _menhir_stack _menhir_s _v) : 'freshtv268)) : 'freshtv270)
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              let (_menhir_env : _menhir_env) = _menhir_env in
              let (_menhir_stack : ('freshtv271 * _menhir_state * 'tv_linear_form) * _menhir_state * 'tv_linear_form) = Obj.magic _menhir_stack in
              ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv272)) : 'freshtv274)) : 'freshtv276)
      | MenhirState44 ->
          let (_menhir_env : _menhir_env) = _menhir_env in
          let (_menhir_stack : ('freshtv285 * _menhir_state * 'tv_linear_form) * _menhir_state * 'tv_linear_form) = Obj.magic _menhir_stack in
          ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
          let _tok = _menhir_env._menhir_token in
          let (_menhir_env : _menhir_env) = _menhir_env in
          let (_menhir_stack : ('freshtv283 * _menhir_state * 'tv_linear_form) * _menhir_state * 'tv_linear_form) = _menhir_stack in
          let (_tok : Tokens.token) = _tok in
          ((match _tok with
          | Tokens.SEMICOLON ->
              let (_menhir_env : _menhir_env) = _menhir_env in
              let (_menhir_stack : ('freshtv279 * _menhir_state * 'tv_linear_form) * _menhir_state * 'tv_linear_form) = Obj.magic _menhir_stack in
              ((let _ = _menhir_discard _menhir_env in
              let (_menhir_env : _menhir_env) = _menhir_env in
              let (_menhir_stack : ('freshtv277 * _menhir_state * 'tv_linear_form) * _menhir_state * 'tv_linear_form) = Obj.magic _menhir_stack in
              ((let ((_menhir_stack, _menhir_s, l1), _, l2) = _menhir_stack in
              let _v : 'tv_ineq = 
# 132 "_parser.mly"
                                                  (
  let lpos = List.rev_map (fun (j, v) -> (j, Pos, v) ) l1 in
  let lneg = List.rev_map (fun (j,v) -> (j, Neg, v) ) l2 in
  List.rev_append lpos lneg
)
# 447 "parser.ml"
               in
              _menhir_goto_ineq _menhir_env _menhir_stack _menhir_s _v) : 'freshtv278)) : 'freshtv280)
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              let (_menhir_env : _menhir_env) = _menhir_env in
              let (_menhir_stack : ('freshtv281 * _menhir_state * 'tv_linear_form) * _menhir_state * 'tv_linear_form) = Obj.magic _menhir_stack in
              ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv282)) : 'freshtv284)) : 'freshtv286)
      | _ ->
          _menhir_fail ()
  
  and _menhir_goto_separated_nonempty_list_COMMA_term_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_separated_nonempty_list_COMMA_term_ -> 'ttv_return =
    fun _menhir_env _menhir_stack _menhir_s _v ->
      match _menhir_s with
      | MenhirState26 ->
          let (_menhir_env : _menhir_env) = _menhir_env in
          let (_menhir_stack : 'freshtv215 * _menhir_state * 'tv_term) = Obj.magic _menhir_stack in
          let (_menhir_s : _menhir_state) = _menhir_s in
          let (_v : 'tv_separated_nonempty_list_COMMA_term_) = _v in
          ((let (_menhir_env : _menhir_env) = _menhir_env in
          let (_menhir_stack : 'freshtv213 * _menhir_state * 'tv_term) = Obj.magic _menhir_stack in
          let (_ : _menhir_state) = _menhir_s in
          let (xs : 'tv_separated_nonempty_list_COMMA_term_) = _v in
          ((let (_menhir_stack, _menhir_s, x) = _menhir_stack in
          let _v : 'tv_separated_nonempty_list_COMMA_term_ = 
# 146 "/usr/share/menhir/standard.mly"
    ( x :: xs )
# 476 "parser.ml"
           in
          _menhir_goto_separated_nonempty_list_COMMA_term_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv214)) : 'freshtv216)
      | MenhirState22 ->
          let (_menhir_env : _menhir_env) = _menhir_env in
          let (_menhir_stack : 'freshtv231) = Obj.magic _menhir_stack in
          let (_menhir_s : _menhir_state) = _menhir_s in
          let (_v : 'tv_separated_nonempty_list_COMMA_term_) = _v in
          ((let (_menhir_env : _menhir_env) = _menhir_env in
          let (_menhir_stack : 'freshtv229) = Obj.magic _menhir_stack in
          let (_menhir_s : _menhir_state) = _menhir_s in
          let (l : 'tv_separated_nonempty_list_COMMA_term_) = _v in
          ((let _v : 'tv_term_list = 
# 104 "_parser.mly"
                                           ( l )
# 491 "parser.ml"
           in
          let (_menhir_env : _menhir_env) = _menhir_env in
          let (_menhir_stack : 'freshtv227) = _menhir_stack in
          let (_menhir_s : _menhir_state) = _menhir_s in
          let (_v : 'tv_term_list) = _v in
          ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
          let (_menhir_env : _menhir_env) = _menhir_env in
          let (_menhir_stack : ('freshtv225 * _menhir_state) * _menhir_state * 'tv_term_list) = Obj.magic _menhir_stack in
          ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
          let _tok = _menhir_env._menhir_token in
          let (_menhir_env : _menhir_env) = _menhir_env in
          let (_menhir_stack : ('freshtv223 * _menhir_state) * _menhir_state * 'tv_term_list) = _menhir_stack in
          let (_tok : Tokens.token) = _tok in
          ((match _tok with
          | Tokens.RPAREN ->
              let (_menhir_env : _menhir_env) = _menhir_env in
              let (_menhir_stack : ('freshtv219 * _menhir_state) * _menhir_state * 'tv_term_list) = Obj.magic _menhir_stack in
              ((let _ = _menhir_discard _menhir_env in
              let (_menhir_env : _menhir_env) = _menhir_env in
              let (_menhir_stack : ('freshtv217 * _menhir_state) * _menhir_state * 'tv_term_list) = Obj.magic _menhir_stack in
              ((let ((_menhir_stack, _menhir_s), _, list) = _menhir_stack in
              let _v : 'tv_linear_form = 
# 100 "_parser.mly"
                                     ((*Printf.printf "linear form, max\n ";*) list)
# 516 "parser.ml"
               in
              _menhir_goto_linear_form _menhir_env _menhir_stack _menhir_s _v) : 'freshtv218)) : 'freshtv220)
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              let (_menhir_env : _menhir_env) = _menhir_env in
              let (_menhir_stack : ('freshtv221 * _menhir_state) * _menhir_state * 'tv_term_list) = Obj.magic _menhir_stack in
              ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv222)) : 'freshtv224)) : 'freshtv226)) : 'freshtv228)) : 'freshtv230)) : 'freshtv232)
      | _ ->
          _menhir_fail ()
  
  and _menhir_goto_separated_nonempty_list_COMMA_num_expr_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_separated_nonempty_list_COMMA_num_expr_ -> 'ttv_return =
    fun _menhir_env _menhir_stack _menhir_s _v ->
      let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
      match _menhir_s with
      | MenhirState50 ->
          let (_menhir_env : _menhir_env) = _menhir_env in
          let (_menhir_stack : ('freshtv207) * _menhir_state * 'tv_separated_nonempty_list_COMMA_num_expr_) = Obj.magic _menhir_stack in
          ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
          let _tok = _menhir_env._menhir_token in
          let (_menhir_env : _menhir_env) = _menhir_env in
          let (_menhir_stack : ('freshtv205) * _menhir_state * 'tv_separated_nonempty_list_COMMA_num_expr_) = _menhir_stack in
          let (_tok : Tokens.token) = _tok in
          ((match _tok with
          | Tokens.SEMICOLON ->
              let (_menhir_env : _menhir_env) = _menhir_env in
              let (_menhir_stack : ('freshtv201) * _menhir_state * 'tv_separated_nonempty_list_COMMA_num_expr_) = Obj.magic _menhir_stack in
              ((let _ = _menhir_discard _menhir_env in
              let (_menhir_env : _menhir_env) = _menhir_env in
              let (_menhir_stack : ('freshtv199) * _menhir_state * 'tv_separated_nonempty_list_COMMA_num_expr_) = Obj.magic _menhir_stack in
              ((let (_menhir_stack, _, l) = _menhir_stack in
              let _v : 'tv_basic_point = 
# 141 "_parser.mly"
                                                                        (l)
# 552 "parser.ml"
               in
              let (_menhir_env : _menhir_env) = _menhir_env in
              let (_menhir_stack : 'freshtv197) = _menhir_stack in
              let (_v : 'tv_basic_point) = _v in
              ((let _menhir_stack = (_menhir_stack, _v) in
              let (_menhir_env : _menhir_env) = _menhir_env in
              let (_menhir_stack : (('freshtv195 * 'tv_objective) * _menhir_state * 'tv_ineqs) * 'tv_basic_point) = Obj.magic _menhir_stack in
              ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              let _tok = _menhir_env._menhir_token in
              let (_menhir_env : _menhir_env) = _menhir_env in
              let (_menhir_stack : (('freshtv193 * 'tv_objective) * _menhir_state * 'tv_ineqs) * 'tv_basic_point) = _menhir_stack in
              let (_tok : Tokens.token) = _tok in
              ((match _tok with
              | Tokens.EOF ->
                  let (_menhir_env : _menhir_env) = _menhir_env in
                  let (_menhir_stack : (('freshtv189 * 'tv_objective) * _menhir_state * 'tv_ineqs) * 'tv_basic_point) = Obj.magic _menhir_stack in
                  ((let (_menhir_env : _menhir_env) = _menhir_env in
                  let (_menhir_stack : (('freshtv187 * 'tv_objective) * _menhir_state * 'tv_ineqs) * 'tv_basic_point) = Obj.magic _menhir_stack in
                  ((let (((_menhir_stack, o), _, cl), point) = _menhir_stack in
                  let _v : (
# 46 "_parser.mly"
       ( (LinearProg.col_index_t * LinearProg.sign_t * Numeric.t) list *
  ((LinearProg.col_index_t * LinearProg.sign_t * Numeric.t) list) list *
  Numeric.t list)
# 577 "parser.ml"
                  ) = 
# 112 "_parser.mly"
                                                   ( (o, cl, point) )
# 581 "parser.ml"
                   in
                  _menhir_goto_main _menhir_env _menhir_stack _v) : 'freshtv188)) : 'freshtv190)
              | _ ->
                  assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                  _menhir_env._menhir_shifted <- (-1);
                  let (_menhir_env : _menhir_env) = _menhir_env in
                  let (_menhir_stack : (('freshtv191 * 'tv_objective) * _menhir_state * 'tv_ineqs) * 'tv_basic_point) = Obj.magic _menhir_stack in
                  ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
                  _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv192)) : 'freshtv194)) : 'freshtv196)) : 'freshtv198)) : 'freshtv200)) : 'freshtv202)
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              let (_menhir_env : _menhir_env) = _menhir_env in
              let (_menhir_stack : ('freshtv203) * _menhir_state * 'tv_separated_nonempty_list_COMMA_num_expr_) = Obj.magic _menhir_stack in
              ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv204)) : 'freshtv206)) : 'freshtv208)
      | MenhirState54 ->
          let (_menhir_env : _menhir_env) = _menhir_env in
          let (_menhir_stack : ('freshtv211 * _menhir_state * 'tv_num_expr) * _menhir_state * 'tv_separated_nonempty_list_COMMA_num_expr_) = Obj.magic _menhir_stack in
          ((let (_menhir_env : _menhir_env) = _menhir_env in
          let (_menhir_stack : ('freshtv209 * _menhir_state * 'tv_num_expr) * _menhir_state * 'tv_separated_nonempty_list_COMMA_num_expr_) = Obj.magic _menhir_stack in
          ((let ((_menhir_stack, _menhir_s, x), _, xs) = _menhir_stack in
          let _v : 'tv_separated_nonempty_list_COMMA_num_expr_ = 
# 146 "/usr/share/menhir/standard.mly"
    ( x :: xs )
# 607 "parser.ml"
           in
          _menhir_goto_separated_nonempty_list_COMMA_num_expr_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv210)) : 'freshtv212)
      | _ ->
          _menhir_fail ()
  
  and _menhir_run8 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_num_expr -> 'ttv_return =
    fun _menhir_env _menhir_stack ->
      let _tok = _menhir_discard _menhir_env in
      let (_menhir_env : _menhir_env) = _menhir_env in
      let (_menhir_stack : 'freshtv185 * _menhir_state * 'tv_num_expr) = _menhir_stack in
      let (_tok : Tokens.token) = _tok in
      ((match _tok with
      | Tokens.LPAREN ->
          _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState8
      | Tokens.MINUS ->
          _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState8
      | Tokens.NUM _v ->
          _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState8 _v
      | _ ->
          assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
          _menhir_env._menhir_shifted <- (-1);
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState8) : 'freshtv186)
  
  and _menhir_run11 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_num_expr -> 'ttv_return =
    fun _menhir_env _menhir_stack ->
      let _tok = _menhir_discard _menhir_env in
      let (_menhir_env : _menhir_env) = _menhir_env in
      let (_menhir_stack : 'freshtv183 * _menhir_state * 'tv_num_expr) = _menhir_stack in
      let (_tok : Tokens.token) = _tok in
      ((match _tok with
      | Tokens.LPAREN ->
          _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState11
      | Tokens.MINUS ->
          _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState11
      | Tokens.NUM _v ->
          _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState11 _v
      | _ ->
          assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
          _menhir_env._menhir_shifted <- (-1);
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState11) : 'freshtv184)
  
  and _menhir_run15 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_num_expr -> 'ttv_return =
    fun _menhir_env _menhir_stack ->
      let _tok = _menhir_discard _menhir_env in
      let (_menhir_env : _menhir_env) = _menhir_env in
      let (_menhir_stack : 'freshtv181 * _menhir_state * 'tv_num_expr) = _menhir_stack in
      let (_tok : Tokens.token) = _tok in
      ((match _tok with
      | Tokens.LPAREN ->
          _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState15
      | Tokens.MINUS ->
          _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState15
      | Tokens.NUM _v ->
          _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState15 _v
      | _ ->
          assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
          _menhir_env._menhir_shifted <- (-1);
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState15) : 'freshtv182)
  
  and _menhir_run13 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_num_expr -> 'ttv_return =
    fun _menhir_env _menhir_stack ->
      let _tok = _menhir_discard _menhir_env in
      let (_menhir_env : _menhir_env) = _menhir_env in
      let (_menhir_stack : 'freshtv179 * _menhir_state * 'tv_num_expr) = _menhir_stack in
      let (_tok : Tokens.token) = _tok in
      ((match _tok with
      | Tokens.LPAREN ->
          _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState13
      | Tokens.MINUS ->
          _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState13
      | Tokens.NUM _v ->
          _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState13 _v
      | _ ->
          assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
          _menhir_env._menhir_shifted <- (-1);
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState13) : 'freshtv180)
  
  and _menhir_goto_term : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_term -> 'ttv_return =
    fun _menhir_env _menhir_stack _menhir_s _v ->
      let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
      match _menhir_s with
      | MenhirState26 | MenhirState22 ->
          let (_menhir_env : _menhir_env) = _menhir_env in
          let (_menhir_stack : 'freshtv173 * _menhir_state * 'tv_term) = Obj.magic _menhir_stack in
          ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
          let _tok = _menhir_env._menhir_token in
          let (_menhir_env : _menhir_env) = _menhir_env in
          let (_menhir_stack : 'freshtv171 * _menhir_state * 'tv_term) = _menhir_stack in
          let (_tok : Tokens.token) = _tok in
          ((match _tok with
          | Tokens.COMMA ->
              let (_menhir_env : _menhir_env) = _menhir_env in
              let (_menhir_stack : 'freshtv165 * _menhir_state * 'tv_term) = Obj.magic _menhir_stack in
              ((let _tok = _menhir_discard _menhir_env in
              let (_menhir_env : _menhir_env) = _menhir_env in
              let (_menhir_stack : 'freshtv163 * _menhir_state * 'tv_term) = _menhir_stack in
              let (_tok : Tokens.token) = _tok in
              ((match _tok with
              | Tokens.LPAREN ->
                  _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState26
              | Tokens.MINUS ->
                  _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState26
              | Tokens.NUM _v ->
                  _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _v
              | Tokens.VAR _v ->
                  _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _v
              | _ ->
                  assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                  _menhir_env._menhir_shifted <- (-1);
                  _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState26) : 'freshtv164)) : 'freshtv166)
          | Tokens.RPAREN ->
              let (_menhir_env : _menhir_env) = _menhir_env in
              let (_menhir_stack : 'freshtv167 * _menhir_state * 'tv_term) = Obj.magic _menhir_stack in
              ((let (_menhir_stack, _menhir_s, x) = _menhir_stack in
              let _v : 'tv_separated_nonempty_list_COMMA_term_ = 
# 144 "/usr/share/menhir/standard.mly"
    ( [ x ] )
# 725 "parser.ml"
               in
              _menhir_goto_separated_nonempty_list_COMMA_term_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv168)
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              let (_menhir_env : _menhir_env) = _menhir_env in
              let (_menhir_stack : 'freshtv169 * _menhir_state * 'tv_term) = Obj.magic _menhir_stack in
              ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv170)) : 'freshtv172)) : 'freshtv174)
      | MenhirState58 | MenhirState44 | MenhirState41 | MenhirState38 | MenhirState35 | MenhirState1 ->
          let (_menhir_env : _menhir_env) = _menhir_env in
          let (_menhir_stack : 'freshtv177 * _menhir_state * 'tv_term) = Obj.magic _menhir_stack in
          ((let (_menhir_env : _menhir_env) = _menhir_env in
          let (_menhir_stack : 'freshtv175 * _menhir_state * 'tv_term) = Obj.magic _menhir_stack in
          ((let (_menhir_stack, _menhir_s, t) = _menhir_stack in
          let _v : 'tv_linear_form = 
# 99 "_parser.mly"
           ((*Printf.printf "linear form, simple term\n ";*) t::[] )
# 744 "parser.ml"
           in
          _menhir_goto_linear_form _menhir_env _menhir_stack _menhir_s _v) : 'freshtv176)) : 'freshtv178)
      | _ ->
          _menhir_fail ()
  
  and _menhir_goto_num_expr : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_num_expr -> 'ttv_return =
    fun _menhir_env _menhir_stack _menhir_s _v ->
      let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
      match _menhir_s with
      | MenhirState6 ->
          let (_menhir_env : _menhir_env) = _menhir_env in
          let (_menhir_stack : ('freshtv89 * _menhir_state) * _menhir_state * 'tv_num_expr) = Obj.magic _menhir_stack in
          ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
          let _tok = _menhir_env._menhir_token in
          let (_menhir_env : _menhir_env) = _menhir_env in
          let (_menhir_stack : ('freshtv87 * _menhir_state) * _menhir_state * 'tv_num_expr) = _menhir_stack in
          let (_tok : Tokens.token) = _tok in
          ((match _tok with
          | Tokens.DIV ->
              _menhir_run13 _menhir_env (Obj.magic _menhir_stack)
          | Tokens.MINUS ->
              _menhir_run15 _menhir_env (Obj.magic _menhir_stack)
          | Tokens.PLUS ->
              _menhir_run11 _menhir_env (Obj.magic _menhir_stack)
          | Tokens.RPAREN ->
              let (_menhir_env : _menhir_env) = _menhir_env in
              let (_menhir_stack : ('freshtv83 * _menhir_state) * _menhir_state * 'tv_num_expr) = Obj.magic _menhir_stack in
              ((let _ = _menhir_discard _menhir_env in
              let (_menhir_env : _menhir_env) = _menhir_env in
              let (_menhir_stack : ('freshtv81 * _menhir_state) * _menhir_state * 'tv_num_expr) = Obj.magic _menhir_stack in
              ((let ((_menhir_stack, _menhir_s), _, e) = _menhir_stack in
              let _v : 'tv_num_expr = 
# 84 "_parser.mly"
    ( e )
# 779 "parser.ml"
               in
              _menhir_goto_num_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv82)) : 'freshtv84)
          | Tokens.TIMES ->
              _menhir_run8 _menhir_env (Obj.magic _menhir_stack)
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              let (_menhir_env : _menhir_env) = _menhir_env in
              let (_menhir_stack : ('freshtv85 * _menhir_state) * _menhir_state * 'tv_num_expr) = Obj.magic _menhir_stack in
              ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv86)) : 'freshtv88)) : 'freshtv90)
      | MenhirState8 ->
          let (_menhir_env : _menhir_env) = _menhir_env in
          let (_menhir_stack : ('freshtv93 * _menhir_state * 'tv_num_expr) * _menhir_state * 'tv_num_expr) = Obj.magic _menhir_stack in
          ((let (_menhir_env : _menhir_env) = _menhir_env in
          let (_menhir_stack : ('freshtv91 * _menhir_state * 'tv_num_expr) * _menhir_state * 'tv_num_expr) = Obj.magic _menhir_stack in
          ((let ((_menhir_stack, _menhir_s, e1), _, e2) = _menhir_stack in
          let _v : 'tv_num_expr = 
# 90 "_parser.mly"
    ( Numeric.mul e1  e2 )
# 800 "parser.ml"
           in
          _menhir_goto_num_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv92)) : 'freshtv94)
      | MenhirState29 | MenhirState11 ->
          let (_menhir_env : _menhir_env) = _menhir_env in
          let (_menhir_stack : ('freshtv101 * _menhir_state * 'tv_num_expr) * _menhir_state * 'tv_num_expr) = Obj.magic _menhir_stack in
          ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
          let _tok = _menhir_env._menhir_token in
          let (_menhir_env : _menhir_env) = _menhir_env in
          let (_menhir_stack : ('freshtv99 * _menhir_state * 'tv_num_expr) * _menhir_state * 'tv_num_expr) = _menhir_stack in
          let (_tok : Tokens.token) = _tok in
          ((match _tok with
          | Tokens.DIV ->
              _menhir_run13 _menhir_env (Obj.magic _menhir_stack)
          | Tokens.TIMES ->
              _menhir_run8 _menhir_env (Obj.magic _menhir_stack)
          | Tokens.COMMA | Tokens.GEQ | Tokens.LEQ | Tokens.MINUS | Tokens.PLUS | Tokens.RPAREN | Tokens.SEMICOLON ->
              let (_menhir_env : _menhir_env) = _menhir_env in
              let (_menhir_stack : ('freshtv95 * _menhir_state * 'tv_num_expr) * _menhir_state * 'tv_num_expr) = Obj.magic _menhir_stack in
              ((let ((_menhir_stack, _menhir_s, e1), _, e2) = _menhir_stack in
              let _v : 'tv_num_expr = 
# 86 "_parser.mly"
    ( Numeric.add e1  e2 )
# 823 "parser.ml"
               in
              _menhir_goto_num_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv96)
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              let (_menhir_env : _menhir_env) = _menhir_env in
              let (_menhir_stack : ('freshtv97 * _menhir_state * 'tv_num_expr) * _menhir_state * 'tv_num_expr) = Obj.magic _menhir_stack in
              ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv98)) : 'freshtv100)) : 'freshtv102)
      | MenhirState13 ->
          let (_menhir_env : _menhir_env) = _menhir_env in
          let (_menhir_stack : ('freshtv105 * _menhir_state * 'tv_num_expr) * _menhir_state * 'tv_num_expr) = Obj.magic _menhir_stack in
          ((let (_menhir_env : _menhir_env) = _menhir_env in
          let (_menhir_stack : ('freshtv103 * _menhir_state * 'tv_num_expr) * _menhir_state * 'tv_num_expr) = Obj.magic _menhir_stack in
          ((let ((_menhir_stack, _menhir_s, e1), _, e2) = _menhir_stack in
          let _v : 'tv_num_expr = 
# 92 "_parser.mly"
    ( Numeric.div e1  e2 )
# 842 "parser.ml"
           in
          _menhir_goto_num_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv104)) : 'freshtv106)
      | MenhirState15 ->
          let (_menhir_env : _menhir_env) = _menhir_env in
          let (_menhir_stack : ('freshtv113 * _menhir_state * 'tv_num_expr) * _menhir_state * 'tv_num_expr) = Obj.magic _menhir_stack in
          ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
          let _tok = _menhir_env._menhir_token in
          let (_menhir_env : _menhir_env) = _menhir_env in
          let (_menhir_stack : ('freshtv111 * _menhir_state * 'tv_num_expr) * _menhir_state * 'tv_num_expr) = _menhir_stack in
          let (_tok : Tokens.token) = _tok in
          ((match _tok with
          | Tokens.DIV ->
              _menhir_run13 _menhir_env (Obj.magic _menhir_stack)
          | Tokens.TIMES ->
              _menhir_run8 _menhir_env (Obj.magic _menhir_stack)
          | Tokens.COMMA | Tokens.GEQ | Tokens.LEQ | Tokens.MINUS | Tokens.PLUS | Tokens.RPAREN | Tokens.SEMICOLON ->
              let (_menhir_env : _menhir_env) = _menhir_env in
              let (_menhir_stack : ('freshtv107 * _menhir_state * 'tv_num_expr) * _menhir_state * 'tv_num_expr) = Obj.magic _menhir_stack in
              ((let ((_menhir_stack, _menhir_s, e1), _, e2) = _menhir_stack in
              let _v : 'tv_num_expr = 
# 88 "_parser.mly"
    ( let ne2 = Numeric.neg e2 in Numeric.add e1 ne2 )
# 865 "parser.ml"
               in
              _menhir_goto_num_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv108)
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              let (_menhir_env : _menhir_env) = _menhir_env in
              let (_menhir_stack : ('freshtv109 * _menhir_state * 'tv_num_expr) * _menhir_state * 'tv_num_expr) = Obj.magic _menhir_stack in
              ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv110)) : 'freshtv112)) : 'freshtv114)
      | MenhirState5 ->
          let (_menhir_env : _menhir_env) = _menhir_env in
          let (_menhir_stack : ('freshtv117 * _menhir_state) * _menhir_state * 'tv_num_expr) = Obj.magic _menhir_stack in
          ((let (_menhir_env : _menhir_env) = _menhir_env in
          let (_menhir_stack : ('freshtv115 * _menhir_state) * _menhir_state * 'tv_num_expr) = Obj.magic _menhir_stack in
          ((let ((_menhir_stack, _menhir_s), _, e) = _menhir_stack in
          let _v : 'tv_num_expr = 
# 94 "_parser.mly"
    ( Numeric.neg e )
# 884 "parser.ml"
           in
          _menhir_goto_num_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv116)) : 'freshtv118)
      | MenhirState3 ->
          let (_menhir_env : _menhir_env) = _menhir_env in
          let (_menhir_stack : ('freshtv125 * _menhir_state * (
# 31 "_parser.mly"
       (LinearProg.col_index_t)
# 892 "parser.ml"
          )) * _menhir_state * 'tv_num_expr) = Obj.magic _menhir_stack in
          ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
          let _tok = _menhir_env._menhir_token in
          let (_menhir_env : _menhir_env) = _menhir_env in
          let (_menhir_stack : ('freshtv123 * _menhir_state * (
# 31 "_parser.mly"
       (LinearProg.col_index_t)
# 900 "parser.ml"
          )) * _menhir_state * 'tv_num_expr) = _menhir_stack in
          let (_tok : Tokens.token) = _tok in
          ((match _tok with
          | Tokens.DIV ->
              _menhir_run13 _menhir_env (Obj.magic _menhir_stack)
          | Tokens.MINUS ->
              _menhir_run15 _menhir_env (Obj.magic _menhir_stack)
          | Tokens.PLUS ->
              _menhir_run11 _menhir_env (Obj.magic _menhir_stack)
          | Tokens.TIMES ->
              _menhir_run8 _menhir_env (Obj.magic _menhir_stack)
          | Tokens.COMMA | Tokens.GEQ | Tokens.LEQ | Tokens.RPAREN | Tokens.SEMICOLON ->
              let (_menhir_env : _menhir_env) = _menhir_env in
              let (_menhir_stack : ('freshtv119 * _menhir_state * (
# 31 "_parser.mly"
       (LinearProg.col_index_t)
# 917 "parser.ml"
              )) * _menhir_state * 'tv_num_expr) = Obj.magic _menhir_stack in
              ((let ((_menhir_stack, _menhir_s, j), _, v) = _menhir_stack in
              let _v : 'tv_term = 
# 75 "_parser.mly"
                            ( (j,v) )
# 923 "parser.ml"
               in
              _menhir_goto_term _menhir_env _menhir_stack _menhir_s _v) : 'freshtv120)
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              let (_menhir_env : _menhir_env) = _menhir_env in
              let (_menhir_stack : ('freshtv121 * _menhir_state * (
# 31 "_parser.mly"
       (LinearProg.col_index_t)
# 933 "parser.ml"
              )) * _menhir_state * 'tv_num_expr) = Obj.magic _menhir_stack in
              ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv122)) : 'freshtv124)) : 'freshtv126)
      | MenhirState19 ->
          let (_menhir_env : _menhir_env) = _menhir_env in
          let (_menhir_stack : ('freshtv133 * _menhir_state * (
# 31 "_parser.mly"
       (LinearProg.col_index_t)
# 942 "parser.ml"
          )) * _menhir_state * 'tv_num_expr) = Obj.magic _menhir_stack in
          ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
          let _tok = _menhir_env._menhir_token in
          let (_menhir_env : _menhir_env) = _menhir_env in
          let (_menhir_stack : ('freshtv131 * _menhir_state * (
# 31 "_parser.mly"
       (LinearProg.col_index_t)
# 950 "parser.ml"
          )) * _menhir_state * 'tv_num_expr) = _menhir_stack in
          let (_tok : Tokens.token) = _tok in
          ((match _tok with
          | Tokens.DIV ->
              _menhir_run13 _menhir_env (Obj.magic _menhir_stack)
          | Tokens.MINUS ->
              _menhir_run15 _menhir_env (Obj.magic _menhir_stack)
          | Tokens.PLUS ->
              _menhir_run11 _menhir_env (Obj.magic _menhir_stack)
          | Tokens.TIMES ->
              _menhir_run8 _menhir_env (Obj.magic _menhir_stack)
          | Tokens.COMMA | Tokens.GEQ | Tokens.LEQ | Tokens.RPAREN | Tokens.SEMICOLON ->
              let (_menhir_env : _menhir_env) = _menhir_env in
              let (_menhir_stack : ('freshtv127 * _menhir_state * (
# 31 "_parser.mly"
       (LinearProg.col_index_t)
# 967 "parser.ml"
              )) * _menhir_state * 'tv_num_expr) = Obj.magic _menhir_stack in
              ((let ((_menhir_stack, _menhir_s, j), _, v) = _menhir_stack in
              let _v : 'tv_term = 
# 76 "_parser.mly"
                             ( let vv = Numeric.neg v in
(*Printf.printf "term  %s\n"  (Numeric.to_string vv);*)
 (j,vv) )
# 975 "parser.ml"
               in
              _menhir_goto_term _menhir_env _menhir_stack _menhir_s _v) : 'freshtv128)
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              let (_menhir_env : _menhir_env) = _menhir_env in
              let (_menhir_stack : ('freshtv129 * _menhir_state * (
# 31 "_parser.mly"
       (LinearProg.col_index_t)
# 985 "parser.ml"
              )) * _menhir_state * 'tv_num_expr) = Obj.magic _menhir_stack in
              ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv130)) : 'freshtv132)) : 'freshtv134)
      | MenhirState58 | MenhirState44 | MenhirState41 | MenhirState38 | MenhirState35 | MenhirState1 | MenhirState22 | MenhirState26 ->
          let (_menhir_env : _menhir_env) = _menhir_env in
          let (_menhir_stack : 'freshtv149 * _menhir_state * 'tv_num_expr) = Obj.magic _menhir_stack in
          ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
          let _tok = _menhir_env._menhir_token in
          let (_menhir_env : _menhir_env) = _menhir_env in
          let (_menhir_stack : 'freshtv147 * _menhir_state * 'tv_num_expr) = _menhir_stack in
          let (_tok : Tokens.token) = _tok in
          ((match _tok with
          | Tokens.DIV ->
              _menhir_run13 _menhir_env (Obj.magic _menhir_stack)
          | Tokens.MINUS ->
              _menhir_run15 _menhir_env (Obj.magic _menhir_stack)
          | Tokens.PLUS ->
              let (_menhir_env : _menhir_env) = _menhir_env in
              let (_menhir_stack : 'freshtv141 * _menhir_state * 'tv_num_expr) = Obj.magic _menhir_stack in
              ((let _tok = _menhir_discard _menhir_env in
              let (_menhir_env : _menhir_env) = _menhir_env in
              let (_menhir_stack : 'freshtv139 * _menhir_state * 'tv_num_expr) = _menhir_stack in
              let (_tok : Tokens.token) = _tok in
              ((match _tok with
              | Tokens.LPAREN ->
                  _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState29
              | Tokens.MINUS ->
                  _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState29
              | Tokens.NUM _v ->
                  _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _v
              | Tokens.VAR _v ->
                  let (_menhir_env : _menhir_env) = _menhir_env in
                  let (_menhir_stack : 'freshtv137 * _menhir_state * 'tv_num_expr) = Obj.magic _menhir_stack in
                  let (_menhir_s : _menhir_state) = MenhirState29 in
                  let (_v : (
# 31 "_parser.mly"
       (LinearProg.col_index_t)
# 1023 "parser.ml"
                  )) = _v in
                  ((let _ = _menhir_discard _menhir_env in
                  let (_menhir_env : _menhir_env) = _menhir_env in
                  let (_menhir_stack : 'freshtv135 * _menhir_state * 'tv_num_expr) = Obj.magic _menhir_stack in
                  let (_ : _menhir_state) = _menhir_s in
                  let (j : (
# 31 "_parser.mly"
       (LinearProg.col_index_t)
# 1032 "parser.ml"
                  )) = _v in
                  ((let (_menhir_stack, _menhir_s, v) = _menhir_stack in
                  let _v : 'tv_term = 
# 72 "_parser.mly"
                            ((j,v) )
# 1038 "parser.ml"
                   in
                  _menhir_goto_term _menhir_env _menhir_stack _menhir_s _v) : 'freshtv136)) : 'freshtv138)
              | _ ->
                  assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                  _menhir_env._menhir_shifted <- (-1);
                  _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState29) : 'freshtv140)) : 'freshtv142)
          | Tokens.TIMES ->
              _menhir_run8 _menhir_env (Obj.magic _menhir_stack)
          | Tokens.COMMA | Tokens.GEQ | Tokens.LEQ | Tokens.RPAREN | Tokens.SEMICOLON ->
              let (_menhir_env : _menhir_env) = _menhir_env in
              let (_menhir_stack : 'freshtv143 * _menhir_state * 'tv_num_expr) = Obj.magic _menhir_stack in
              ((let (_menhir_stack, _menhir_s, v) = _menhir_stack in
              let _v : 'tv_term = 
# 71 "_parser.mly"
               ( (Affine, v) )
# 1054 "parser.ml"
               in
              _menhir_goto_term _menhir_env _menhir_stack _menhir_s _v) : 'freshtv144)
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              let (_menhir_env : _menhir_env) = _menhir_env in
              let (_menhir_stack : 'freshtv145 * _menhir_state * 'tv_num_expr) = Obj.magic _menhir_stack in
              ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv146)) : 'freshtv148)) : 'freshtv150)
      | MenhirState54 | MenhirState50 ->
          let (_menhir_env : _menhir_env) = _menhir_env in
          let (_menhir_stack : 'freshtv161 * _menhir_state * 'tv_num_expr) = Obj.magic _menhir_stack in
          ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
          let _tok = _menhir_env._menhir_token in
          let (_menhir_env : _menhir_env) = _menhir_env in
          let (_menhir_stack : 'freshtv159 * _menhir_state * 'tv_num_expr) = _menhir_stack in
          let (_tok : Tokens.token) = _tok in
          ((match _tok with
          | Tokens.COMMA ->
              let (_menhir_env : _menhir_env) = _menhir_env in
              let (_menhir_stack : 'freshtv153 * _menhir_state * 'tv_num_expr) = Obj.magic _menhir_stack in
              ((let _tok = _menhir_discard _menhir_env in
              let (_menhir_env : _menhir_env) = _menhir_env in
              let (_menhir_stack : 'freshtv151 * _menhir_state * 'tv_num_expr) = _menhir_stack in
              let (_tok : Tokens.token) = _tok in
              ((match _tok with
              | Tokens.LPAREN ->
                  _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState54
              | Tokens.MINUS ->
                  _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState54
              | Tokens.NUM _v ->
                  _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _v
              | _ ->
                  assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                  _menhir_env._menhir_shifted <- (-1);
                  _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState54) : 'freshtv152)) : 'freshtv154)
          | Tokens.DIV ->
              _menhir_run13 _menhir_env (Obj.magic _menhir_stack)
          | Tokens.MINUS ->
              _menhir_run15 _menhir_env (Obj.magic _menhir_stack)
          | Tokens.PLUS ->
              _menhir_run11 _menhir_env (Obj.magic _menhir_stack)
          | Tokens.TIMES ->
              _menhir_run8 _menhir_env (Obj.magic _menhir_stack)
          | Tokens.SEMICOLON ->
              let (_menhir_env : _menhir_env) = _menhir_env in
              let (_menhir_stack : 'freshtv155 * _menhir_state * 'tv_num_expr) = Obj.magic _menhir_stack in
              ((let (_menhir_stack, _menhir_s, x) = _menhir_stack in
              let _v : 'tv_separated_nonempty_list_COMMA_num_expr_ = 
# 144 "/usr/share/menhir/standard.mly"
    ( [ x ] )
# 1106 "parser.ml"
               in
              _menhir_goto_separated_nonempty_list_COMMA_num_expr_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv156)
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              let (_menhir_env : _menhir_env) = _menhir_env in
              let (_menhir_stack : 'freshtv157 * _menhir_state * 'tv_num_expr) = Obj.magic _menhir_stack in
              ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv158)) : 'freshtv160)) : 'freshtv162)
  
  and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
    fun _menhir_env _menhir_stack _menhir_s ->
      match _menhir_s with
      | MenhirState58 ->
          let (_menhir_env : _menhir_env) = _menhir_env in
          let (_menhir_stack : 'freshtv43 * _menhir_state * 'tv_ineq) = Obj.magic _menhir_stack in
          ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv44)
      | MenhirState54 ->
          let (_menhir_env : _menhir_env) = _menhir_env in
          let (_menhir_stack : 'freshtv45 * _menhir_state * 'tv_num_expr) = Obj.magic _menhir_stack in
          ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv46)
      | MenhirState50 ->
          let (_menhir_env : _menhir_env) = _menhir_env in
          let (_menhir_stack : 'freshtv47) = Obj.magic _menhir_stack in
          (raise _eRR : 'freshtv48)
      | MenhirState44 ->
          let (_menhir_env : _menhir_env) = _menhir_env in
          let (_menhir_stack : 'freshtv49 * _menhir_state * 'tv_linear_form) = Obj.magic _menhir_stack in
          ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv50)
      | MenhirState41 ->
          let (_menhir_env : _menhir_env) = _menhir_env in
          let (_menhir_stack : 'freshtv51 * _menhir_state * 'tv_linear_form) = Obj.magic _menhir_stack in
          ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv52)
      | MenhirState38 ->
          let (_menhir_env : _menhir_env) = _menhir_env in
          let (_menhir_stack : 'freshtv53 * 'tv_objective) = Obj.magic _menhir_stack in
          (raise _eRR : 'freshtv54)
      | MenhirState35 ->
          let (_menhir_env : _menhir_env) = _menhir_env in
          let (_menhir_stack : 'freshtv55) = Obj.magic _menhir_stack in
          (raise _eRR : 'freshtv56)
      | MenhirState29 ->
          let (_menhir_env : _menhir_env) = _menhir_env in
          let (_menhir_stack : 'freshtv57 * _menhir_state * 'tv_num_expr) = Obj.magic _menhir_stack in
          ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv58)
      | MenhirState26 ->
          let (_menhir_env : _menhir_env) = _menhir_env in
          let (_menhir_stack : 'freshtv59 * _menhir_state * 'tv_term) = Obj.magic _menhir_stack in
          ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv60)
      | MenhirState22 ->
          let (_menhir_env : _menhir_env) = _menhir_env in
          let (_menhir_stack : 'freshtv61 * _menhir_state) = Obj.magic _menhir_stack in
          ((let (_menhir_stack, _menhir_s) = _menhir_stack in
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv62)
      | MenhirState19 ->
          let (_menhir_env : _menhir_env) = _menhir_env in
          let (_menhir_stack : 'freshtv63 * _menhir_state * (
# 31 "_parser.mly"
       (LinearProg.col_index_t)
# 1172 "parser.ml"
          )) = Obj.magic _menhir_stack in
          ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv64)
      | MenhirState15 ->
          let (_menhir_env : _menhir_env) = _menhir_env in
          let (_menhir_stack : 'freshtv65 * _menhir_state * 'tv_num_expr) = Obj.magic _menhir_stack in
          ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv66)
      | MenhirState13 ->
          let (_menhir_env : _menhir_env) = _menhir_env in
          let (_menhir_stack : 'freshtv67 * _menhir_state * 'tv_num_expr) = Obj.magic _menhir_stack in
          ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv68)
      | MenhirState11 ->
          let (_menhir_env : _menhir_env) = _menhir_env in
          let (_menhir_stack : 'freshtv69 * _menhir_state * 'tv_num_expr) = Obj.magic _menhir_stack in
          ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv70)
      | MenhirState8 ->
          let (_menhir_env : _menhir_env) = _menhir_env in
          let (_menhir_stack : 'freshtv71 * _menhir_state * 'tv_num_expr) = Obj.magic _menhir_stack in
          ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv72)
      | MenhirState6 ->
          let (_menhir_env : _menhir_env) = _menhir_env in
          let (_menhir_stack : 'freshtv73 * _menhir_state) = Obj.magic _menhir_stack in
          ((let (_menhir_stack, _menhir_s) = _menhir_stack in
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv74)
      | MenhirState5 ->
          let (_menhir_env : _menhir_env) = _menhir_env in
          let (_menhir_stack : 'freshtv75 * _menhir_state) = Obj.magic _menhir_stack in
          ((let (_menhir_stack, _menhir_s) = _menhir_stack in
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv76)
      | MenhirState3 ->
          let (_menhir_env : _menhir_env) = _menhir_env in
          let (_menhir_stack : 'freshtv77 * _menhir_state * (
# 31 "_parser.mly"
       (LinearProg.col_index_t)
# 1211 "parser.ml"
          )) = Obj.magic _menhir_stack in
          ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv78)
      | MenhirState1 ->
          let (_menhir_env : _menhir_env) = _menhir_env in
          let (_menhir_stack : 'freshtv79) = Obj.magic _menhir_stack in
          (raise _eRR : 'freshtv80)
  
  and _menhir_run2 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 31 "_parser.mly"
       (LinearProg.col_index_t)
# 1223 "parser.ml"
  ) -> 'ttv_return =
    fun _menhir_env _menhir_stack _menhir_s _v ->
      let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
      let _tok = _menhir_discard _menhir_env in
      let (_menhir_env : _menhir_env) = _menhir_env in
      let (_menhir_stack : 'freshtv41 * _menhir_state * (
# 31 "_parser.mly"
       (LinearProg.col_index_t)
# 1232 "parser.ml"
      )) = _menhir_stack in
      let (_tok : Tokens.token) = _tok in
      ((match _tok with
      | Tokens.MINUS ->
          let (_menhir_env : _menhir_env) = _menhir_env in
          let (_menhir_stack : 'freshtv31 * _menhir_state * (
# 31 "_parser.mly"
       (LinearProg.col_index_t)
# 1241 "parser.ml"
          )) = Obj.magic _menhir_stack in
          ((let _tok = _menhir_discard _menhir_env in
          let (_menhir_env : _menhir_env) = _menhir_env in
          let (_menhir_stack : 'freshtv29 * _menhir_state * (
# 31 "_parser.mly"
       (LinearProg.col_index_t)
# 1248 "parser.ml"
          )) = _menhir_stack in
          let (_tok : Tokens.token) = _tok in
          ((match _tok with
          | Tokens.LPAREN ->
              _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState19
          | Tokens.MINUS ->
              _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState19
          | Tokens.NUM _v ->
              _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState19 _v
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState19) : 'freshtv30)) : 'freshtv32)
      | Tokens.PLUS ->
          let (_menhir_env : _menhir_env) = _menhir_env in
          let (_menhir_stack : 'freshtv35 * _menhir_state * (
# 31 "_parser.mly"
       (LinearProg.col_index_t)
# 1267 "parser.ml"
          )) = Obj.magic _menhir_stack in
          ((let _tok = _menhir_discard _menhir_env in
          let (_menhir_env : _menhir_env) = _menhir_env in
          let (_menhir_stack : 'freshtv33 * _menhir_state * (
# 31 "_parser.mly"
       (LinearProg.col_index_t)
# 1274 "parser.ml"
          )) = _menhir_stack in
          let (_tok : Tokens.token) = _tok in
          ((match _tok with
          | Tokens.LPAREN ->
              _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState3
          | Tokens.MINUS ->
              _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState3
          | Tokens.NUM _v ->
              _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState3 _v
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState3) : 'freshtv34)) : 'freshtv36)
      | Tokens.COMMA | Tokens.GEQ | Tokens.LEQ | Tokens.RPAREN | Tokens.SEMICOLON ->
          let (_menhir_env : _menhir_env) = _menhir_env in
          let (_menhir_stack : 'freshtv37 * _menhir_state * (
# 31 "_parser.mly"
       (LinearProg.col_index_t)
# 1293 "parser.ml"
          )) = Obj.magic _menhir_stack in
          ((let (_menhir_stack, _menhir_s, j) = _menhir_stack in
          let _v : 'tv_term = 
# 74 "_parser.mly"
          ( (j, Numeric.zero) )
# 1299 "parser.ml"
           in
          _menhir_goto_term _menhir_env _menhir_stack _menhir_s _v) : 'freshtv38)
      | _ ->
          assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
          _menhir_env._menhir_shifted <- (-1);
          let (_menhir_env : _menhir_env) = _menhir_env in
          let (_menhir_stack : 'freshtv39 * _menhir_state * (
# 31 "_parser.mly"
       (LinearProg.col_index_t)
# 1309 "parser.ml"
          )) = Obj.magic _menhir_stack in
          ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv40)) : 'freshtv42)
  
  and _menhir_run4 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 30 "_parser.mly"
       (string)
# 1317 "parser.ml"
  ) -> 'ttv_return =
    fun _menhir_env _menhir_stack _menhir_s _v ->
      let _ = _menhir_discard _menhir_env in
      let (_menhir_env : _menhir_env) = _menhir_env in
      let (_menhir_stack : 'freshtv27) = Obj.magic _menhir_stack in
      let (_menhir_s : _menhir_state) = _menhir_s in
      let (s : (
# 30 "_parser.mly"
       (string)
# 1327 "parser.ml"
      )) = _v in
      ((let _v : 'tv_num_expr = 
# 82 "_parser.mly"
    ( Numeric.of_string s  )
# 1332 "parser.ml"
       in
      _menhir_goto_num_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv28)
  
  and _menhir_run5 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
    fun _menhir_env _menhir_stack _menhir_s ->
      let _menhir_stack = (_menhir_stack, _menhir_s) in
      let _tok = _menhir_discard _menhir_env in
      let (_menhir_env : _menhir_env) = _menhir_env in
      let (_menhir_stack : 'freshtv25 * _menhir_state) = _menhir_stack in
      let (_tok : Tokens.token) = _tok in
      ((match _tok with
      | Tokens.LPAREN ->
          _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState5
      | Tokens.MINUS ->
          _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState5
      | Tokens.NUM _v ->
          _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState5 _v
      | _ ->
          assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
          _menhir_env._menhir_shifted <- (-1);
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState5) : 'freshtv26)
  
  and _menhir_run21 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
    fun _menhir_env _menhir_stack _menhir_s ->
      let _menhir_stack = (_menhir_stack, _menhir_s) in
      let _tok = _menhir_discard _menhir_env in
      let (_menhir_env : _menhir_env) = _menhir_env in
      let (_menhir_stack : 'freshtv23 * _menhir_state) = _menhir_stack in
      let (_tok : Tokens.token) = _tok in
      ((match _tok with
      | Tokens.LPAREN ->
          let (_menhir_env : _menhir_env) = _menhir_env in
          let (_menhir_stack : 'freshtv19 * _menhir_state) = Obj.magic _menhir_stack in
          ((let _tok = _menhir_discard _menhir_env in
          let (_menhir_env : _menhir_env) = _menhir_env in
          let (_menhir_stack : 'freshtv17 * _menhir_state) = _menhir_stack in
          let (_tok : Tokens.token) = _tok in
          ((match _tok with
          | Tokens.LPAREN ->
              _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState22
          | Tokens.MINUS ->
              _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState22
          | Tokens.NUM _v ->
              _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState22 _v
          | Tokens.VAR _v ->
              _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState22 _v
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState22) : 'freshtv18)) : 'freshtv20)
      | _ ->
          assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
          _menhir_env._menhir_shifted <- (-1);
          let (_menhir_env : _menhir_env) = _menhir_env in
          let (_menhir_stack : 'freshtv21 * _menhir_state) = Obj.magic _menhir_stack in
          ((let (_menhir_stack, _menhir_s) = _menhir_stack in
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv22)) : 'freshtv24)
  
  and _menhir_run6 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
    fun _menhir_env _menhir_stack _menhir_s ->
      let _menhir_stack = (_menhir_stack, _menhir_s) in
      let _tok = _menhir_discard _menhir_env in
      let (_menhir_env : _menhir_env) = _menhir_env in
      let (_menhir_stack : 'freshtv15 * _menhir_state) = _menhir_stack in
      let (_tok : Tokens.token) = _tok in
      ((match _tok with
      | Tokens.LPAREN ->
          _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState6
      | Tokens.MINUS ->
          _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState6
      | Tokens.NUM _v ->
          _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState6 _v
      | _ ->
          assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
          _menhir_env._menhir_shifted <- (-1);
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState6) : 'freshtv16)
  
  and _menhir_discard : _menhir_env -> Tokens.token =
    fun _menhir_env ->
      let lexbuf = _menhir_env._menhir_lexbuf in
      let _tok = _menhir_env._menhir_lexer lexbuf in
      _menhir_env._menhir_token <- _tok;
      _menhir_env._menhir_startp <- lexbuf.Lexing.lex_start_p;
      _menhir_env._menhir_endp <- lexbuf.Lexing.lex_curr_p;
      let shifted = Pervasives.(+) _menhir_env._menhir_shifted 1 in
      if Pervasives.(>=) shifted 0 then
        _menhir_env._menhir_shifted <- shifted;
      _tok
  
  and main : (Lexing.lexbuf -> Tokens.token) -> Lexing.lexbuf -> (
# 46 "_parser.mly"
       ( (LinearProg.col_index_t * LinearProg.sign_t * Numeric.t) list *
  ((LinearProg.col_index_t * LinearProg.sign_t * Numeric.t) list) list *
  Numeric.t list)
# 1427 "parser.ml"
  ) =
    fun lexer lexbuf ->
      let _menhir_env =
        let (lexer : Lexing.lexbuf -> Tokens.token) = lexer in
        let (lexbuf : Lexing.lexbuf) = lexbuf in
        ((let _tok = lexer lexbuf in
        {
          _menhir_lexer = lexer;
          _menhir_lexbuf = lexbuf;
          _menhir_token = _tok;
          _menhir_startp = lexbuf.Lexing.lex_start_p;
          _menhir_endp = lexbuf.Lexing.lex_curr_p;
          _menhir_shifted = max_int;
          }) : _menhir_env)
      in
      Obj.magic (let (_menhir_env : _menhir_env) = _menhir_env in
      let (_menhir_stack : 'freshtv13) = () in
      ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
      let _tok = _menhir_env._menhir_token in
      let (_menhir_env : _menhir_env) = _menhir_env in
      let (_menhir_stack : 'freshtv11) = _menhir_stack in
      let (_tok : Tokens.token) = _tok in
      ((match _tok with
      | Tokens.MAXIMIZE ->
          let (_menhir_env : _menhir_env) = _menhir_env in
          let (_menhir_stack : 'freshtv3) = Obj.magic _menhir_stack in
          ((let _tok = _menhir_discard _menhir_env in
          let (_menhir_env : _menhir_env) = _menhir_env in
          let (_menhir_stack : 'freshtv1) = _menhir_stack in
          let (_tok : Tokens.token) = _tok in
          ((match _tok with
          | Tokens.LPAREN ->
              _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState35
          | Tokens.MAX ->
              _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState35
          | Tokens.MINUS ->
              _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState35
          | Tokens.NUM _v ->
              _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _v
          | Tokens.VAR _v ->
              _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _v
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState35) : 'freshtv2)) : 'freshtv4)
      | Tokens.MINIMIZE ->
          let (_menhir_env : _menhir_env) = _menhir_env in
          let (_menhir_stack : 'freshtv7) = Obj.magic _menhir_stack in
          ((let _tok = _menhir_discard _menhir_env in
          let (_menhir_env : _menhir_env) = _menhir_env in
          let (_menhir_stack : 'freshtv5) = _menhir_stack in
          let (_tok : Tokens.token) = _tok in
          ((match _tok with
          | Tokens.LPAREN ->
              _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState1
          | Tokens.MAX ->
              _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState1
          | Tokens.MINUS ->
              _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState1
          | Tokens.NUM _v ->
              _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState1 _v
          | Tokens.VAR _v ->
              _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState1 _v
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState1) : 'freshtv6)) : 'freshtv8)
      | _ ->
          assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
          _menhir_env._menhir_shifted <- (-1);
          let (_menhir_env : _menhir_env) = _menhir_env in
          let (_menhir_stack : 'freshtv9) = Obj.magic _menhir_stack in
          (raise _eRR : 'freshtv10)) : 'freshtv12)) : 'freshtv14))
  
  



end
