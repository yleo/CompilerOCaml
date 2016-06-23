%{
(* --- préambule: ici du code Caml --- *)

open Expr

%}

%token <string> VAR
%token PPOINT
%token EOL

%start demande
%type <string> demande

%%
    /* --- début des règles de grammaire --- */

demande:                       /* à droite, les valeurs associées */
  | VAR EOL
                                            { $1 }
  | EOL
                                            { "" }
  | PPOINT EOL                              { ".." }
;
