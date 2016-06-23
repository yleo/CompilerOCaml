
type arithm =
    Int of int
  | Add of arithm*arithm
  | Sub of arithm*arithm
  | Times of arithm*arithm
  | Div of arithm*arithm
  | Var of var
  | App of string*(arithm list)
and var = 
    Simple of string
  | Multiple of var*arithm

type arithm_or_array =
  | Int_sup of arithm
  | Vector of arithm_or_array array

type typing =
  | Inttype
  | Arraytype of typing*(int*int)

type printable =
    PInt of arithm
  | PString of string
      
type op =
    Equal
  | Neq
  | Leq
  | Lneq
  | Geq
  | Gneq

type cond =
    Or of cond*cond
  | And of cond*cond
  | Xor of cond*cond
  | Not of cond
  | Op of op*arithm*arithm

type cmd =
    Seq of cmd list
  | Ifte of cond*cmd*cmd
  | While of cond*cmd
  | For of var*arithm*arithm*cmd
  | Print of printable
  | Assign of var * arithm

type arg = 
    Val of string*typing
  | Ref of string

type definition =
    DefVal of string * typing * (arithm_or_array option)
  | DefFunc of string * arg list * definition list * cmd

type prgm = definition list * cmd 





(************************************************)
(*     Fonctions d'impression                   *)
(************************************************)



let printn a = 
  print_string("|");
  for i = 0 to a-1 do
    print_string("-")
  done

let k = ref 0

let ps = print_string 

let rec iter l f = 
  match l with
    | [] -> ()
    | t::q -> begin f t; iter q f end

let rec iter l f = 
  match l with
    | [] -> ()
    | t::q -> begin f t; iter q f end

let rec print_arithm expr = 
  match expr with 
    | Int(i) -> print_string "Int";print_int i
    | Add(e1,e2) -> 
	begin 
	  ps "Add(";
	  print_arithm e1;
	  ps ",";
	  print_arithm e2;
	  ps ")"
	end
    | Sub(e1,e2) -> 
	begin 
	  ps "Sub(";
	  print_arithm e1;
	  ps ",";
	  print_arithm e2;
	  ps ")"
	end
    | Div(e1,e2) -> 
	begin 
	  ps "Div(";
	  print_arithm e1;
	  ps ",";
	  print_arithm e2;
	  ps ")"
	end
    | Times(e1,e2) -> 
	begin 
	  ps "Times(";
	  print_arithm e1;
	  ps ",";
	  print_arithm e2;
	  ps ")"
	end
    | Var(s) -> 
	begin
	  ps "Var(";
	  print_var s;
	  ps ")"
	end
    | App(f,l) ->
	begin
	  ps (f^"(");
	  aux l;
	  ps ")"
	end
and print_var = function
  |Simple(s)-> 
     begin
       ps s
     end
  |Multiple(a,b)-> 
     begin
       print_var a;
       ps "[";
       print_arithm b;
       ps "]"
     end
and aux l = match l with
    [] -> ()
  |[s]-> print_arithm s
  |s::q -> print_arithm s; ps ","; aux q
      

let rec print_arithm_array expr = 
  match expr with
    |Int_sup(ari)-> print_arithm ari
    |Vector(t)->  
       begin 
	 ps "V(";
	 print_arithm_array (t.(0));
	 for i = 1 to (Array.length t) -1 do
	   ps ",";
	   print_arithm_array (t.(i))
	 done;
	 ps ")"
       end

let rec print_typing t = match t with
  | Inttype -> ps "Inttype"
  | Arraytype(typing,(i,j)) -> 
      begin 
	ps "Arraytype(";
	print_typing typing;
	ps ",";
	print_int i;
	ps ",";
	print_int j;
	ps ")"
      end

let print_arg arg = match arg with
    Val(var,typing) -> 
      begin
	ps "Val(";
	ps var;
	ps ":";
	print_typing typing;
	ps ")";
	ps ","
      end
  | Ref(var) ->
      begin
	ps "Ref(";
	ps var;
	ps ":";
	ps "Inttype";
	ps ")";
	ps ","
      end

let print_printable p = match p with
    PInt(arithm) -> 
      begin 
	ps "PInt(";
	print_arithm arithm;
	ps ")"
      end
  | PString(s) ->
      begin 
	ps "PString(";
	ps s;
	ps ")"
      end


let print_op o = match o with
  | Equal -> ps "="
  | Neq -> ps "!="
  | Leq -> ps "<="
  | Lneq -> ps "<"
  | Geq -> ps ">="
  | Gneq -> ps ">"

let rec print_cond c = match c with
    Or(c1,c2) -> 
      begin
	ps "Or(";
	print_cond(c1);
	ps ",";
	print_cond(c2);
	ps ")"
      end
  | And(c1,c2) ->
      begin
	ps "And(";
	print_cond(c1);
	ps ",";
	print_cond(c2);
	ps ")"
      end
  | Xor(c1,c2) ->
      begin
	ps "Xor(";
	print_cond(c1);
	ps ",";
	print_cond(c2);
	ps ")"
      end
  | Not(c1) ->
      begin
	ps "Nor(";
	print_cond c1;
	ps ")"
      end
  | Op(o,e1,e2) ->
      begin
	print_arithm e1;
	print_op o;
	print_arithm e2
      end


let rec print_cmd cmd = match cmd with
    Seq([])-> ()
  |Seq(l) -> 
      begin 
	printn (!k);
	ps "Seq(";
	print_newline();
	k:=!k+2;
	iter l (fun s -> (print_cmd s;));
	k:=!k-2;
	printn (!k);
	ps ")";
	print_newline()
      end
  | Ifte(cond,cmd1,cmd2) ->
      begin
	printn (!k);
	ps "ifte(";
	print_cond cond;
	ps ",";
	print_newline();
	k:=!k+2;
	print_cmd cmd1;
	print_cmd cmd2;
	k:=!k-2;
	printn (!k);
	ps ")";
	print_newline()
      end
  | While(cond,cmd) -> 
      begin
	printn (!k);
	ps "while(";
	print_cond cond;
	k:=!k+2;
	print_cmd cmd;
	k:=!k-2;
	printn (!k);
	ps ")";
	print_newline()
      end
  | For(var,arithm1,arithm2,cmd) -> 
      begin
	printn (!k);
	ps "for(";
	print_var var;
	ps ",";
	print_arithm arithm1;
	ps ",";
	print_arithm arithm2;
	ps ",";
	print_newline();
	k:=!k+2;
	print_cmd cmd;
	k:=!k-2;
	printn (!k);
	ps ")";
	print_newline()
      end
  | Print(p) -> 
      begin
	printn (!k);
	ps "Print(";
	print_printable p;
	ps ")";
	print_newline()
      end
  | Assign(var,arithm) ->
      begin
	printn (!k);
	ps "Assign(";
	print_var var;
	ps ",";
	print_arithm arithm;
	ps ")";
	print_newline()
      end
	
let rec print_definition def = match def with
    DefVal(s1,t,arithm) -> 
      begin
	printn (!k);
	ps "DefVal(";
	ps s1;
	ps ",";
	print_typing t;
	ps ",";
	begin
	  match arithm with
	    |None -> ps "None"
	    |Some(s) -> print_arithm_array s
	end;
	ps ")";
	print_newline()
      end
  | DefFunc(s1,lv,deflist,cmd) -> 
      begin
	printn (!k);
	ps "Deffunc(";
	ps s1;
	ps ",";
	iter lv print_arg;
	k:=!k+2;
	print_newline();
	iter deflist print_definition;
	print_cmd cmd;
	k:=!k-2;
	printn (!k);
	ps ")";
	print_newline()
      end

let print_prgm p = match p with 
  | dl,cmd  -> 
      begin
	ps "<-- PROGRAM -->";
	print_newline();
	k:=!k+2;
	iter dl (print_definition);
	print_cmd cmd
      end
