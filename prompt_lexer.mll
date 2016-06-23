{
open Prompt_parser;;
exception Fin_de_fichier
}

let noun    = ['a'-'z''A'-'Z']['a'-'z''A'-'Z''0'-'9']*

rule token = parse
    (* gestion des alentours *)
  | eof             { token lexbuf }             (* fin du fichier *)
  | [' ' '\t'] { token lexbuf }    (* on saute : blancs, saut de ligne et tab *)
      
  | '\n'             { EOL }
  | ".."             { PPOINT }
  | noun as k       { VAR(k) }                       

      
