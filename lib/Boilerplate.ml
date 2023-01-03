(**
   Boilerplate to be used as a template when mapping the clojure CST
   to another type of tree.
*)

(* Disable warnings against unused variables *)
[@@@warning "-26-27"]

(* Disable warning against unused 'rec' *)
[@@@warning "-39"]

type env = unit

let token (env : env) (_tok : Tree_sitter_run.Token.t) =
  failwith "not implemented"

let blank (env : env) () =
  failwith "not implemented"

let todo (env : env) _ =
   failwith "not implemented"

let map_auto_res_mark (env : env) (tok : CST.auto_res_mark) =
  (* auto_res_mark *) token env tok

let map_comment (env : env) (tok : CST.comment) =
  (* comment *) token env tok

let map_kwd_lit (env : env) (tok : CST.kwd_lit) =
  (* kwd_lit *) token env tok

let map_char_lit (env : env) (tok : CST.char_lit) =
  (* char_lit *) token env tok

let map_nil_lit (env : env) (tok : CST.nil_lit) =
  (* nil_lit *) token env tok

let map_tok_dquot_rep_pat_0d044a8_rep_bslash_pat_5058f1a_rep_pat_0d044a8_dquot (env : env) (tok : CST.tok_dquot_rep_pat_0d044a8_rep_bslash_pat_5058f1a_rep_pat_0d044a8_dquot) =
  (* tok_dquot_rep_pat_0d044a8_rep_bslash_pat_5058f1a_rep_pat_0d044a8_dquot *) token env tok

let map_tok_pat_0a702c4_rep_choice_pat_0a702c4 (env : env) (tok : CST.tok_pat_0a702c4_rep_choice_pat_0a702c4) =
  (* tok_pat_0a702c4_rep_choice_pat_0a702c4 *) token env tok

let map_str_lit (env : env) (tok : CST.str_lit) =
  (* str_lit *) token env tok

let map_ws (env : env) (tok : CST.ws) =
  (* ws *) token env tok

let map_num_lit (env : env) (tok : CST.num_lit) =
  (* num_lit *) token env tok

let map_bool_lit (env : env) (tok : CST.bool_lit) =
  (* bool_lit *) token env tok

let rec map_anon_choice_read_cond_lit_137feb9 (env : env) (x : CST.anon_choice_read_cond_lit_137feb9) =
  (match x with
  | `Read_cond_lit x -> map_read_cond_lit env x
  | `Map_lit x -> map_map_lit env x
  | `Str_lit tok -> (* str_lit *) token env tok
  | `Kwd_lit tok -> (* kwd_lit *) token env tok
  | `Sym_lit x -> map_sym_lit env x
  )

and map_bare_list_lit (env : env) ((v1, v2, v3) : CST.bare_list_lit) =
  let v1 = (* "(" *) token env v1 in
  let v2 = map_source env v2 in
  let v3 = (* ")" *) token env v3 in
  todo env (v1, v2, v3)

and map_bare_map_lit (env : env) ((v1, v2, v3) : CST.bare_map_lit) =
  let v1 = (* "{" *) token env v1 in
  let v2 = map_source env v2 in
  let v3 = (* "}" *) token env v3 in
  todo env (v1, v2, v3)

and map_bare_set_lit (env : env) ((v1, v2, v3, v4) : CST.bare_set_lit) =
  let v1 = (* "#" *) token env v1 in
  let v2 = (* "{" *) token env v2 in
  let v3 = map_source env v3 in
  let v4 = (* "}" *) token env v4 in
  todo env (v1, v2, v3, v4)

and map_bare_vec_lit (env : env) ((v1, v2, v3) : CST.bare_vec_lit) =
  let v1 = (* "[" *) token env v1 in
  let v2 = map_source env v2 in
  let v3 = (* "]" *) token env v3 in
  todo env (v1, v2, v3)

and map_form (env : env) (x : CST.form) =
  (match x with
  | `Num_lit tok -> (* num_lit *) token env tok
  | `Kwd_lit tok -> (* kwd_lit *) token env tok
  | `Str_lit tok -> (* str_lit *) token env tok
  | `Char_lit tok -> (* char_lit *) token env tok
  | `Nil_lit tok -> (* nil_lit *) token env tok
  | `Bool_lit tok -> (* bool_lit *) token env tok
  | `Sym_lit x -> map_sym_lit env x
  | `List_lit x -> map_list_lit env x
  | `Map_lit x -> map_map_lit env x
  | `Vec_lit (v1, v2) ->
      let v1 = List.map (map_metadata_lit env) v1 in
      let v2 = map_bare_vec_lit env v2 in
      todo env (v1, v2)
  | `Set_lit (v1, v2) ->
      let v1 = List.map (map_metadata_lit env) v1 in
      let v2 = map_bare_set_lit env v2 in
      todo env (v1, v2)
  | `Anon_fn_lit (v1, v2, v3) ->
      let v1 = List.map (map_metadata_lit env) v1 in
      let v2 = (* "#" *) token env v2 in
      let v3 = map_bare_list_lit env v3 in
      todo env (v1, v2, v3)
  | `Regex_lit (v1, v2) ->
      let v1 = (* "#" *) token env v1 in
      let v2 =
        (* tok_dquot_rep_pat_0d044a8_rep_bslash_pat_5058f1a_rep_pat_0d044a8_dquot *) token env v2
      in
      todo env (v1, v2)
  | `Read_cond_lit x -> map_read_cond_lit env x
  | `Spli_read_cond_lit (v1, v2, v3, v4) ->
      let v1 = List.map (map_metadata_lit env) v1 in
      let v2 = (* "#?@" *) token env v2 in
      let v3 = List.map (token env (* ws *)) v3 in
      let v4 = map_bare_list_lit env v4 in
      todo env (v1, v2, v3, v4)
  | `Ns_map_lit (v1, v2, v3, v4, v5) ->
      let v1 = List.map (map_metadata_lit env) v1 in
      let v2 = (* "#" *) token env v2 in
      let v3 =
        (match v3 with
        | `Auto_res_mark tok -> (* auto_res_mark *) token env tok
        | `Kwd_lit tok -> (* kwd_lit *) token env tok
        )
      in
      let v4 = List.map (map_gap env) v4 in
      let v5 = map_bare_map_lit env v5 in
      todo env (v1, v2, v3, v4, v5)
  | `Var_quot_lit (v1, v2, v3, v4) ->
      let v1 = List.map (map_metadata_lit env) v1 in
      let v2 = (* "#'" *) token env v2 in
      let v3 = List.map (map_gap env) v3 in
      let v4 = map_form env v4 in
      todo env (v1, v2, v3, v4)
  | `Sym_val_lit (v1, v2, v3) ->
      let v1 = (* "##" *) token env v1 in
      let v2 = List.map (map_gap env) v2 in
      let v3 = map_sym_lit env v3 in
      todo env (v1, v2, v3)
  | `Eval_lit (v1, v2, v3, v4) ->
      let v1 = List.map (map_metadata_lit env) v1 in
      let v2 = (* "#=" *) token env v2 in
      let v3 = List.map (map_gap env) v3 in
      let v4 =
        (match v4 with
        | `List_lit x -> map_list_lit env x
        | `Read_cond_lit x -> map_read_cond_lit env x
        | `Sym_lit x -> map_sym_lit env x
        )
      in
      todo env (v1, v2, v3, v4)
  | `Tagged_or_ctor_lit (v1, v2, v3, v4, v5, v6) ->
      let v1 = List.map (map_metadata_lit env) v1 in
      let v2 = (* "#" *) token env v2 in
      let v3 = List.map (map_gap env) v3 in
      let v4 = map_sym_lit env v4 in
      let v5 = List.map (map_gap env) v5 in
      let v6 = map_form env v6 in
      todo env (v1, v2, v3, v4, v5, v6)
  | `Dere_lit (v1, v2, v3, v4) ->
      let v1 = List.map (map_metadata_lit env) v1 in
      let v2 = (* "@" *) token env v2 in
      let v3 = List.map (map_gap env) v3 in
      let v4 = map_form env v4 in
      todo env (v1, v2, v3, v4)
  | `Quot_lit (v1, v2, v3, v4) ->
      let v1 = List.map (map_metadata_lit env) v1 in
      let v2 = (* "'" *) token env v2 in
      let v3 = List.map (map_gap env) v3 in
      let v4 = map_form env v4 in
      todo env (v1, v2, v3, v4)
  | `Syn_quot_lit (v1, v2, v3, v4) ->
      let v1 = List.map (map_metadata_lit env) v1 in
      let v2 = (* "`" *) token env v2 in
      let v3 = List.map (map_gap env) v3 in
      let v4 = map_form env v4 in
      todo env (v1, v2, v3, v4)
  | `Unqu_spli_lit (v1, v2, v3, v4) ->
      let v1 = List.map (map_metadata_lit env) v1 in
      let v2 = (* "~@" *) token env v2 in
      let v3 = List.map (map_gap env) v3 in
      let v4 = map_form env v4 in
      todo env (v1, v2, v3, v4)
  | `Unqu_lit (v1, v2, v3, v4) ->
      let v1 = List.map (map_metadata_lit env) v1 in
      let v2 = (* "~" *) token env v2 in
      let v3 = List.map (map_gap env) v3 in
      let v4 = map_form env v4 in
      todo env (v1, v2, v3, v4)
  )

and map_gap (env : env) (x : CST.gap) =
  (match x with
  | `Ws tok -> (* ws *) token env tok
  | `Comm tok -> (* comment *) token env tok
  | `Dis_expr (v1, v2, v3) ->
      let v1 = (* "#_" *) token env v1 in
      let v2 = List.map (map_gap env) v2 in
      let v3 = map_form env v3 in
      todo env (v1, v2, v3)
  )

and map_list_lit (env : env) ((v1, v2) : CST.list_lit) =
  let v1 = List.map (map_metadata_lit env) v1 in
  let v2 = map_bare_list_lit env v2 in
  todo env (v1, v2)

and map_map_lit (env : env) ((v1, v2) : CST.map_lit) =
  let v1 = List.map (map_metadata_lit env) v1 in
  let v2 = map_bare_map_lit env v2 in
  todo env (v1, v2)

and map_meta_lit (env : env) ((v1, v2, v3) : CST.meta_lit) =
  let v1 = (* "^" *) token env v1 in
  let v2 = List.map (map_gap env) v2 in
  let v3 = map_anon_choice_read_cond_lit_137feb9 env v3 in
  todo env (v1, v2, v3)

and map_metadata_lit (env : env) ((v1, v2) : CST.metadata_lit) =
  let v1 =
    (match v1 with
    | `Meta_lit x -> map_meta_lit env x
    | `Old_meta_lit x -> map_old_meta_lit env x
    )
  in
  let v2 =
    (match v2 with
    | Some xs -> List.map (map_gap env) xs
    | None -> todo env ())
  in
  todo env (v1, v2)

and map_old_meta_lit (env : env) ((v1, v2, v3) : CST.old_meta_lit) =
  let v1 = (* "#^" *) token env v1 in
  let v2 = List.map (map_gap env) v2 in
  let v3 = map_anon_choice_read_cond_lit_137feb9 env v3 in
  todo env (v1, v2, v3)

and map_read_cond_lit (env : env) ((v1, v2, v3, v4) : CST.read_cond_lit) =
  let v1 = List.map (map_metadata_lit env) v1 in
  let v2 = (* "#?" *) token env v2 in
  let v3 = List.map (token env (* ws *)) v3 in
  let v4 = map_bare_list_lit env v4 in
  todo env (v1, v2, v3, v4)

and map_source (env : env) (xs : CST.source) =
  List.map (fun x ->
    (match x with
    | `Form x -> map_form env x
    | `Gap x -> map_gap env x
    )
  ) xs

and map_sym_lit (env : env) ((v1, v2) : CST.sym_lit) =
  let v1 = List.map (map_metadata_lit env) v1 in
  let v2 =
    (* tok_pat_0a702c4_rep_choice_pat_0a702c4 *) token env v2
  in
  todo env (v1, v2)
