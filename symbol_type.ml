open Expr
open Expr_int

type symbol_table = 
      Root of ((string,descriptor) Hashtbl.t)
    | Locale of symbol_table*((string,descriptor) Hashtbl.t)
and descriptor = 
      Int of descr_int 
    | Array of descr_array
    | Function of descr_fun
and access = Temp of int | Stack of int
and passage =
      By_Value
    | By_Reference
    | Local_Var
and descr_int = { 
  argtype_int:passage ;
  num_arg_int:int;
  location:string ; 
  absolute_name_int:int ;
  mutable used_by_descendance:bool ; 
  mutable access_int:access option}
and descr_fun = {
  arg:Expr.arg list ;
  number_arg :int; 
  parent:symbol_table ;
  absolute_name_fun:int ; 
  mutable offset:int ; 
  mutable body: instr option ; 
  mutable access_func:access option ; 
  mutable used_by_desc_fun:bool;
  mutable printed:bool;
  mutable reg_used:int list;
  mutable correspondance : Expr_int.exp array option
}
and descr_array = {
  argtype_array:passage ; 
  typing:Expr.typing ; 
  num_arg_array:int; 
  dimension:(int*int) list; 
  absolute_name_array:int ; 
  mutable stack:int option ; 
  location_array:string;
  mutable access_array:access option}


let ps = print_string
let string_of_bool b = if b then "vrai" else "faux"

let rec new_path i l = match l with
  |[] -> []
  |t::q -> if i=0 then l else new_path (i-1) q

let print_elem a position chemin name i = match a with 
  |Int(d) ->  
     begin
       ps "Int :\n";
       ps "\tArgtype_int: ";
       ps (match d.argtype_int with
	     |By_Value -> "By_Value\n"
	     |By_Reference -> "By_Reference\n"
	     |Local_Var -> "Local_Var\n");
       ps ("\tnum_arg_int: "^(string_of_int d.num_arg_int)^"\n");
       ps ("\tused_by_descendance: "^(string_of_bool (d.used_by_descendance))^"\n");
       ps ("\tlocation: "^(d.location)^"\n");
       ps ("\tabsolute_name_int: "^(string_of_int (d.absolute_name_int))^"\n");
     end
  |Array(d) -> 
     begin
       ps "Array : \n";
       ps "\targtype_array: ";
       ps (match d.argtype_array with
	     |By_Value -> "By_Value\n"
	     |By_Reference -> "By_Reference\n"
	     |Local_Var -> "Local_Var\n");
       ps "\ttyping: ";
       Expr.print_typing (d.typing);
       ps "\n";
       ps ("\tnum_arg_array: "^(string_of_int (d.num_arg_array))^"\n");
       ps ("\tabsolute_name_array: "^(string_of_int (d.absolute_name_array))^"\n")
     end
  |Function(d) -> 
     begin
       ps "Function : \n";
       ps "\targ: ";
       Expr.iter (d.arg) (Expr.print_arg);
       ps"\n";
       ps ("\tabsolute_name_fun: "^(string_of_int (d.absolute_name_fun))^"\n");
       ps "body : \n";
       begin
	 match d.body with
	     Some(code) ->
	       print_instr code;
       end;
       position := d.parent;
       chemin := name::(new_path (i-1) (!chemin))
     end


let rec find_var_path symbtbl name i = 
  match symbtbl with
      Root(htbl) -> 
	begin
 	  try Hashtbl.find htbl name , i
	  with
	    | Not_found -> failwith (name^" n'est pas defini")
	end
    | Locale(previous,htbl) ->
	begin
 	  try Hashtbl.find htbl name , i
	  with
	    | Not_found -> find_var_path previous name (i+1)
	end

