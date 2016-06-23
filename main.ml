open Expr
open Parser
open Lexer
open Symbol_table
open Symbol_type
open Analyse
open Prompt_parser
open Prompt_lexer
open Expr_int
open Geninter
open Hashtbl
open Gencode

let lexbuf = Lexing.from_channel stdin
let parse () = Parser.prgm Lexer.token lexbuf
let parse_symbtbl () = Prompt_parser.demande Prompt_lexer.token lexbuf

let rec print_chemin l = match l with
    [] -> print_string ">"
  | t::q -> print_string t; print_string "/";print_chemin q


let rec iter l f = match l with
    [] -> []
  | t::q -> (f t)::(iter q f)
      



let calc  =
(*  if Array.length Sys.argv > 1 then
    begin
      if Sys.argv.(1) <> "-d" then
	print_string ("Usage : "^Sys.argv.(0)^" [-d]\n et non "^Sys.argv.(0)^" "^Sys.argv.(1)^" ou je ne sais quoi d'autre\n")
    end
  else *)             (* RESERVE AU DEBUG !!*)
    begin
      let result = parse () in
      let symbtbl = Symbol_table.create result in
      let code_inter = Geninter.compile_inter result symbtbl in
      let rien = Analyse.find_var symbtbl "program" in
	match rien with
	    Function(descriptorr) ->
	      descriptorr.body <- Some code_inter;
	      Gencode.tospim code_inter symbtbl;
	      print_newline();
(*	      if Array.length Sys.argv = 1 then  (* RESERVE AU DEBUG !!! *)
		()
	      else if Array.length Sys.argv = 2 then
		begin
		  (*	  Expr_int.print_instr code_inter;*)
		  print_newline();
		  let current = ref symbtbl in
		  let chemin = ref ["program"] in
		    while true do
		      try
			print_chemin (List.rev(!chemin)) ;
			flush stdout;
			let var = parse_symbtbl() in
			  if var = "" then ()
			  else if var = ".." then
			    begin
			      match (!current) with
				|Root(_) ->failwith "deja à la racine \n"
				|Locale(prev, htbl) -> current := prev;
				    chemin := begin match (!chemin) with
				      |[] -> failwith "deja à la racine \n"
				      |t::q -> q end
			    end
			  else if var = "help" then begin print_string ".. pour remonter au papa,\n<nom> pour les infos sur la variable nommée nom\n";
			    flush stdout end
			  else
			    let descr,i = find_var_path (!current) var 1 in 
			      print_elem descr current chemin var i;
			      flush stdout
		      with
			|Failure s -> begin print_string (s^"\n");flush stdout end
		    done
		end		  *)
    end
      
