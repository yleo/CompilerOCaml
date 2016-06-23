type token =
  | VAR of (string)
  | PPOINT
  | EOL

open Parsing;;
# 2 "prompt_parser.mly"
(* --- préambule: ici du code Caml --- *)

open Expr

# 13 "prompt_parser.ml"
let yytransl_const = [|
  258 (* PPOINT *);
  259 (* EOL *);
    0|]

let yytransl_block = [|
  257 (* VAR *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\001\000\000\000"

let yylen = "\002\000\
\002\000\001\000\002\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\002\000\004\000\001\000\003\000"

let yydgoto = "\002\000\
\006\000"

let yysindex = "\002\000\
\255\254\000\000\001\255\002\255\000\000\000\000\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000"

let yytablesize = 5
let yytable = "\003\000\
\004\000\005\000\001\000\007\000\008\000"

let yycheck = "\001\001\
\002\001\003\001\001\000\003\001\003\001"

let yynames_const = "\
  PPOINT\000\
  EOL\000\
  "

let yynames_block = "\
  VAR\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 20 "prompt_parser.mly"
                                            ( _1 )
# 67 "prompt_parser.ml"
               : string))
; (fun __caml_parser_env ->
    Obj.repr(
# 22 "prompt_parser.mly"
                                            ( "" )
# 73 "prompt_parser.ml"
               : string))
; (fun __caml_parser_env ->
    Obj.repr(
# 23 "prompt_parser.mly"
                                            ( ".." )
# 79 "prompt_parser.ml"
               : string))
(* Entry demande *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let demande (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : string)
