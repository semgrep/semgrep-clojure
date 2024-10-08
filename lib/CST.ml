(* Generated by ocaml-tree-sitter. *)
(*
   clojure grammar

   entrypoint: source
*)

open! Sexplib.Conv
open Tree_sitter_run

type comment = Token.t

type ws = Token.t

type auto_res_mark = Token.t

type char_lit = Token.t

type tok_pat_0a702c4_rep_choice_pat_0a702c4 = Token.t

type str_lit = Token.t

type nil_lit = Token.t

type kwd_lit = Token.t

type tok_dquot_rep_pat_0d044a8_rep_bslash_pat_5058f1a_rep_pat_0d044a8_dquot =
  Token.t

type num_lit = Token.t

type bool_lit = Token.t

type anon_choice_read_cond_lit_137feb9 = [
    `Read_cond_lit of read_cond_lit
  | `Map_lit of map_lit
  | `Str_lit of str_lit (*tok*)
  | `Kwd_lit of kwd_lit (*tok*)
  | `Sym_lit of sym_lit
]

and bare_list_lit = (Token.t (* "(" *) * source * Token.t (* ")" *))

and bare_map_lit = (Token.t (* "{" *) * source * Token.t (* "}" *))

and bare_set_lit = (
    Token.t (* "#" *) * Token.t (* "{" *) * source * Token.t (* "}" *)
)

and bare_vec_lit = (Token.t (* "[" *) * source * Token.t (* "]" *))

and form = [
    `Num_lit of num_lit (*tok*)
  | `Kwd_lit of kwd_lit (*tok*)
  | `Str_lit of str_lit (*tok*)
  | `Char_lit of char_lit (*tok*)
  | `Nil_lit of nil_lit (*tok*)
  | `Bool_lit of bool_lit (*tok*)
  | `Sym_lit of sym_lit
  | `List_lit of list_lit
  | `Map_lit of map_lit
  | `Vec_lit of (metadata_lit list (* zero or more *) * bare_vec_lit)
  | `Set_lit of (metadata_lit list (* zero or more *) * bare_set_lit)
  | `Anon_fn_lit of (
        metadata_lit list (* zero or more *)
      * Token.t (* "#" *)
      * bare_list_lit
    )
  | `Regex_lit of (
        Token.t (* "#" *)
      * tok_dquot_rep_pat_0d044a8_rep_bslash_pat_5058f1a_rep_pat_0d044a8_dquot
    )
  | `Read_cond_lit of read_cond_lit
  | `Spli_read_cond_lit of (
        metadata_lit list (* zero or more *)
      * Token.t (* "#?@" *)
      * ws (*tok*) list (* zero or more *)
      * bare_list_lit
    )
  | `Ns_map_lit of (
        metadata_lit list (* zero or more *)
      * Token.t (* "#" *)
      * [
            `Auto_res_mark of auto_res_mark (*tok*)
          | `Kwd_lit of kwd_lit (*tok*)
        ]
      * gap list (* zero or more *)
      * bare_map_lit
    )
  | `Var_quot_lit of (
        metadata_lit list (* zero or more *)
      * Token.t (* "#'" *)
      * gap list (* zero or more *)
      * form
    )
  | `Sym_val_lit of (
        Token.t (* "##" *)
      * gap list (* zero or more *)
      * sym_lit
    )
  | `Eval_lit of (
        metadata_lit list (* zero or more *)
      * Token.t (* "#=" *)
      * gap list (* zero or more *)
      * [
            `List_lit of list_lit
          | `Read_cond_lit of read_cond_lit
          | `Sym_lit of sym_lit
        ]
    )
  | `Tagged_or_ctor_lit of (
        metadata_lit list (* zero or more *)
      * Token.t (* "#" *)
      * gap list (* zero or more *)
      * sym_lit
      * gap list (* zero or more *)
      * form
    )
  | `Dere_lit of (
        metadata_lit list (* zero or more *)
      * Token.t (* "@" *)
      * gap list (* zero or more *)
      * form
    )
  | `Quot_lit of (
        metadata_lit list (* zero or more *)
      * Token.t (* "'" *)
      * gap list (* zero or more *)
      * form
    )
  | `Syn_quot_lit of (
        metadata_lit list (* zero or more *)
      * Token.t (* "`" *)
      * gap list (* zero or more *)
      * form
    )
  | `Unqu_spli_lit of (
        metadata_lit list (* zero or more *)
      * Token.t (* "~@" *)
      * gap list (* zero or more *)
      * form
    )
  | `Unqu_lit of (
        metadata_lit list (* zero or more *)
      * Token.t (* "~" *)
      * gap list (* zero or more *)
      * form
    )
]

and gap = [
    `Ws of ws (*tok*)
  | `Comm of comment (*tok*)
  | `Dis_expr of (Token.t (* "#_" *) * gap list (* zero or more *) * form)
]

and list_lit = (metadata_lit list (* zero or more *) * bare_list_lit)

and map_lit = (metadata_lit list (* zero or more *) * bare_map_lit)

and meta_lit = (
    Token.t (* "^" *)
  * gap list (* zero or more *)
  * anon_choice_read_cond_lit_137feb9
)

and metadata_lit = (
    [ `Meta_lit of meta_lit | `Old_meta_lit of old_meta_lit ]
  * gap list (* zero or more *) option
)

and old_meta_lit = (
    Token.t (* "#^" *)
  * gap list (* zero or more *)
  * anon_choice_read_cond_lit_137feb9
)

and read_cond_lit = (
    metadata_lit list (* zero or more *)
  * Token.t (* "#?" *)
  * ws (*tok*) list (* zero or more *)
  * bare_list_lit
)

and source = [ `Form of form | `Gap of gap ] list (* zero or more *)

and sym_lit = (
    metadata_lit list (* zero or more *)
  * tok_pat_0a702c4_rep_choice_pat_0a702c4
)

type regex_lit (* inlined *) = (
    Token.t (* "#" *)
  * tok_dquot_rep_pat_0d044a8_rep_bslash_pat_5058f1a_rep_pat_0d044a8_dquot
)

type anon_fn_lit (* inlined *) = (
    metadata_lit list (* zero or more *)
  * Token.t (* "#" *)
  * bare_list_lit
)

type derefing_lit (* inlined *) = (
    metadata_lit list (* zero or more *)
  * Token.t (* "@" *)
  * gap list (* zero or more *)
  * form
)

type dis_expr (* inlined *) = (
    Token.t (* "#_" *)
  * gap list (* zero or more *)
  * form
)

type evaling_lit (* inlined *) = (
    metadata_lit list (* zero or more *)
  * Token.t (* "#=" *)
  * gap list (* zero or more *)
  * [
        `List_lit of list_lit
      | `Read_cond_lit of read_cond_lit
      | `Sym_lit of sym_lit
    ]
)

type ns_map_lit (* inlined *) = (
    metadata_lit list (* zero or more *)
  * Token.t (* "#" *)
  * [ `Auto_res_mark of auto_res_mark (*tok*) | `Kwd_lit of kwd_lit (*tok*) ]
  * gap list (* zero or more *)
  * bare_map_lit
)

type quoting_lit (* inlined *) = (
    metadata_lit list (* zero or more *)
  * Token.t (* "'" *)
  * gap list (* zero or more *)
  * form
)

type set_lit (* inlined *) = (
    metadata_lit list (* zero or more *)
  * bare_set_lit
)

type splicing_read_cond_lit (* inlined *) = (
    metadata_lit list (* zero or more *)
  * Token.t (* "#?@" *)
  * ws (*tok*) list (* zero or more *)
  * bare_list_lit
)

type sym_val_lit (* inlined *) = (
    Token.t (* "##" *)
  * gap list (* zero or more *)
  * sym_lit
)

type syn_quoting_lit (* inlined *) = (
    metadata_lit list (* zero or more *)
  * Token.t (* "`" *)
  * gap list (* zero or more *)
  * form
)

type tagged_or_ctor_lit (* inlined *) = (
    metadata_lit list (* zero or more *)
  * Token.t (* "#" *)
  * gap list (* zero or more *)
  * sym_lit
  * gap list (* zero or more *)
  * form
)

type unquote_splicing_lit (* inlined *) = (
    metadata_lit list (* zero or more *)
  * Token.t (* "~@" *)
  * gap list (* zero or more *)
  * form
)

type unquoting_lit (* inlined *) = (
    metadata_lit list (* zero or more *)
  * Token.t (* "~" *)
  * gap list (* zero or more *)
  * form
)

type var_quoting_lit (* inlined *) = (
    metadata_lit list (* zero or more *)
  * Token.t (* "#'" *)
  * gap list (* zero or more *)
  * form
)

type vec_lit (* inlined *) = (
    metadata_lit list (* zero or more *)
  * bare_vec_lit
)

type extra

type extras = extra list
