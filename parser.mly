%{
(* --- préambule: ici du code Caml --- *)

open Expr

%}

%token PRGM FUNCTION
%token BEGIN END
%token <int> INT
%token <string> VAR
%token <string> STR
%token PLUS TIMES SUB DIV 
%token POINT COMMA SEMICOLON DB /* . , ; : */
%token EQUAL GEQ GNEQ LNEQ LEQ NEQ /* = >= > < <= <> */
%token DEF /* represente var dans par exemple "var x,y" */
%token LPAREN RPAREN LSQBR RSQBR
%token EOL EOF
%token INTEGER ARRAY OF 

%token AND OR NOT XOR


%token OUTPUT IF THEN ELSE QUOTE MINUS


%token WRITELN
%token FOR TO    /* le DO ligne d'après */
%token WHILE DO

%left PLUS SUB
%left TIMES DIV
%left OR
%left AND

%nonassoc NO
%nonassoc EQUAL GEQ GNEQ LNEQ LEQ NEQ

%nonassoc THEN
%nonassoc ELSE

%start prgm
%type <Expr.prgm> prgm

%%
    /* --- début des règles de grammaire --- */

prgm:                       /* à droite, les valeurs associées */
  | PRGM VAR LPAREN OUTPUT RPAREN SEMICOLON defs BEGIN seq END POINT
                                            { $7,Seq($9) }
;
defs:
                                            { []   }
  | def SEMICOLON defs                      { $1::$3 }
;
def:
    DEF VAR DB typing                        { DefVal($2,$4,None) } 
  | DEF VAR DB typing EQUAL arithm_or_array  { DefVal($2,$4,Some($6)) } 
  | FUNCTION VAR LPAREN larg RPAREN DB INTEGER SEMICOLON defs BEGIN seq END
                                             { DefFunc($2,$4,$9,Seq($11)) }
;
arithm_or_array:
    arithm                                   { Int_sup($1) }
  | LSQBR lsupervector RSQBR                 { Vector(Array.of_list $2) }
;
lsupervector:
                                               { []   }
  | arithm_or_array                             { [$1] }
  | arithm_or_array SEMICOLON lsupervector      { $1::$3 }
;
larg:
                                             { [] }
  | VAR DB typing                            { [Val ($1,$3)] }
  | VAR DB typing SEMICOLON larg                 { (Val ($1,$3))::$5 }
  | DEF VAR DB INTEGER                        { [Ref ($2)] }
  | DEF VAR DB INTEGER SEMICOLON larg             { (Ref ($2))::$6 }
;

typing:
    INTEGER                                   { Inttype }
  | ARRAY length OF typing            { Arraytype($4,$2)  }
;
length:
                                            { (0,0)}
  | LSQBR INT POINT POINT INT RSQBR         { ($2,$5) }
;
seq:
                                            { [] }
  | cmd                                     { [$1] }
  | cmd SEMICOLON seq                       { $1::$3 }
;
cond:
    LPAREN cond RPAREN                      { $2 }
  | arithm op arithm                        { Op($2,$1,$3) }
  | LPAREN cond RPAREN OR LPAREN cond RPAREN 
                                            { Or($2,$6) }
  | LPAREN cond RPAREN AND LPAREN cond RPAREN 
                                            { And($2,$6) }
  | LPAREN cond RPAREN XOR LPAREN cond RPAREN 
                                            { Xor($2,$6) }
  | NOT cond                                { Not($2) }
;
op:
    LEQ                                     { Leq }
  | LNEQ                                    { Lneq }
  | GEQ                                     { Geq }
  | GNEQ                                    { Gneq }
  | EQUAL                                   { Equal }
  | NEQ                                     { Neq }

cmd:
    BEGIN seq END                           { Seq($2) }
  | IF cond THEN cmd ELSE cmd
                                            { Ifte($2,$4,$6) }
  | IF cond THEN cmd 
                                            { Ifte($2,$4,Seq([])) } 
  | WHILE cond DO cmd                       { While($2,$4) }
  | FOR var DB EQUAL arithm TO arithm DO cmd
                                            { For($2,$5,$7,$9) }
  | WRITELN LPAREN arithm RPAREN            { Print(PInt($3)) }
  | WRITELN LPAREN STR RPAREN               { Print(PString($3)) }
  | var DB EQUAL arithm                     { Assign($1,$4) }
;
/*il faut pouvoir assigner les éléments du tableau*/
arithm:
    INT                                     { Int($1) }
  | LPAREN arithm RPAREN                    { $2 }
  | arithm PLUS arithm                      { Add($1,$3) }
  | arithm SUB arithm                       { Sub($1,$3) }
  | arithm TIMES arithm                     { Times($1,$3) }
  | arithm DIV arithm                       { Div($1,$3) }
  | var                                     { Var($1) }
  | VAR LPAREN larithm RPAREN               { App($1,$3) }
;
larithm:
                                            { [] }
  | arithm                                  { [$1] }
  | arithm COMMA larithm                    { $1::$3 }
;
var:
   VAR                                     { Simple($1) }
 | var LSQBR arithm RSQBR                { Multiple($1,$3) }

;
