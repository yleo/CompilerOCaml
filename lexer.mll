{
open Parser;;
}

let nat     = ['0'-'9']+
let noun    = ['a'-'z''A'-'Z']['a'-'z''A'-'Z''0'-'9']*
let str     = ['"']['\n' '\x20' '\x21' '\x23' - '\xFF']*['"']


rule token = parse
    (* gestion des alentours *)
  | eof             { EOF }             (* fin du fichier *)
  | [' ' '\t' '\n'] { token lexbuf }    (* on saute : blancs, saut de ligne et tab *)
      
  (* Opérateurs arithmétiques *)	
  | '+'             { PLUS }
  | '*'             { TIMES }
  | '-'             { SUB }
  | '/'             { DIV }
      
  (* Opérateurs de comparaisons *)
  | "="             { EQUAL }
  | "<>"            { NEQ }
  | "<="            { LEQ }
  | ">="            { GEQ }
  | ">"             { GNEQ }
  | "<"             { LNEQ }
      
  (* Opérateurs logiques *)
  | "and"            { AND }
  | "or"             { OR }
  | "not"            { NOT }
  | "xor"            { XOR }
      
  (* Ponctuation *)
  | '('             { LPAREN }
  | ')'             { RPAREN }
  | '['             { LSQBR }
  | ']'             { RSQBR }
  | "."             { POINT }
  | ";"             { SEMICOLON }
  | ","             { COMMA }
  | ":"             { DB }
  | '"'             { QUOTE }
      
  (* Instructions *)
  | "if"            { IF }
  | "then"          { THEN }
  | "else"          { ELSE }
      
  | "while"         { WHILE }
  | "for"           { FOR }
  | "do"            { DO }
  | "to"            { TO }
      
  (* Structuration, ca se dit ? *)
  | "program"       { PRGM }
  | "function"      { FUNCTION }
  | "begin"         { BEGIN }
  | "end"           { END }
  | "output"        { OUTPUT }

  (* Typage et affectations *)
  | "var"           { DEF } 
  | "integer"       { INTEGER }
  | "array"       { ARRAY }
  | "of"       { OF }
      (*| ":="            { AFFECT }*)
      (*| "bool"         { BOOLEAN }*)
      
  (* Fonctions *)
  | "writeln"       { WRITELN }

  (* Noms et variables *)
  | noun as k       { VAR(k) }                       
  | nat as k        { INT(int_of_string k) }
  | str as k        { STR(k) }    
