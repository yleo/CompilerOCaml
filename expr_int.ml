
type exp = 
    CONST of int
  | TEMP of register (* utilisé dans le code intermediaire *)
  | REG of int (* utilise apres la coloration *)
  | STACK of int (* utilise apres la coloration *)
  | MEM of exp
  | BINOP of binop * exp * exp
(*  | SP
  | FB*)
  | CALL of string * exp list
(*  | AA  (* pour le retour d'une fonction... *)*)

and instr =
    SEQ of instr list
  | CJUMP of relop * exp * exp * int * int
  | JUMP of int
  | MOVE of exp * exp
  | LABEL of int
  | PUSH of exp
  | PRINTSTRING of string
  | PRINTINT
  | NOP

and binop = ADD | SUB | TIMES | DIV

and relop = 
    EQUAL
  | NEQ
  | LEQ
  | LNEQ
  | GEQ
  | GNEQ

and register = int

(* ce qui est permi :
MOVE(dest, prov) ou :
  dest = REG | STACK | MEM(REG|STACK)
  prov = CONST | REG | STACK | ( MEM | BINOP si dest<>MEM) | CALL
...
*)



(************************************************)
(*     Fonctions d'impression                   *)
(************************************************)


let ps = prerr_string  

let rec iter l f =  
  match l with
    | [] -> () 
    | t::q -> begin f t; iter q f end 

let rec print_reg reg =  
  prerr_int(reg) 

let rec print_relop relop = 
  match relop with 
      EQUAL -> ps "EQUAL,"
    | NEQ -> ps "NEQ,"
    | LEQ -> ps "LEQ,"
    | LNEQ -> ps "LNEQ,"
    | GEQ -> ps "GEQ,"
    | GNEQ -> ps "GNEQ,"

let rec print_binop binop = 
  match binop with 
      ADD -> ps "ADD,"
    | SUB -> ps "SUB,"
    | TIMES -> ps "TIMES,"
    | DIV -> ps "DIV,"

let rec print_expr expr =  
  begin 
    match expr with 
      |CONST(i) ->  
	 begin
	   ps "CONST("; 
	   prerr_int(i);
	   ps ")" 
	 end 
      | TEMP(i) ->  
	  begin
	    ps "TEMP("; 
	    print_reg(i);
	    ps ")" 
	  end
      | REG(i) ->  
	  begin
	    ps "REG("; 
	    print_reg(i);
	    ps ")" 
	  end
      | STACK(i) ->  
	  begin
	    ps "STACK("; 
	    print_reg(i);
	    ps ")" 
	  end
      |MEM(e) ->  
	 begin
	   ps "MEM("; 
	   print_expr(e);
	   ps ")" 
	 end 
      |BINOP(binop, e1, e2) ->  
	 begin
	   ps "BINOP("; 
	   print_binop binop;
	   print_expr e1; 
	   ps ","; 
	   print_expr e2; 
	   ps ")"	     
	 end  
(*      |SP ->  
	 begin
	   ps "SP"
	 end
      |FB ->  
	 begin
	   ps "FB" 
	 end
      |AA ->  
	 begin
	   ps "AA" 
	 end	    *)
      |CALL(s,lreg) ->   
	 begin
	   ps ("CALL("^s^",") ;
	   iter lreg print_expr;
	   ps ("ENDCALL("^s^")") ;
	 end 
      |_ -> failwith "Erreur : Je ne peux afficher cette expression" 
  end
  
(*print_list_instr list_instr;;   *)

let rec print_instr instr = 
   begin
    match instr with 
      |SEQ([]) -> ()
      |SEQ(list_instr) -> 
	   print_list_instr list_instr;
      |CJUMP(relop, e1, e2, i1, i2) -> 
	 begin
	   ps "CJUMP(";
	   print_relop relop;
	   print_expr e1;
	   ps ",";
	   print_expr e2; 
	   ps ",";
	   prerr_int(i1);
	   ps ",";
	   prerr_int(i2);
	   ps ")"
	 end
      |JUMP(e) -> 
	 begin
	   ps "JUMP(";
	   prerr_int(e);
	   ps ")";
	 end 
      |MOVE(e1, e2) -> 
	 begin
	   ps "MOVE(";
	  
	   print_expr e1;
	   ps ",";
	
	   print_expr e2;    
	   ps ")"
	 end  
      |LABEL(i) -> 
	 begin
	   ps "LABEL(";
	   prerr_int(i);
	   ps ")"
	 end
      |PUSH(expr) -> 
	 begin
	   ps "PUSH(";
	 
	   print_expr expr;
	  
	   ps ")"
	 end
      |PRINTSTRING(a) -> 
	 begin
	   ps "PRINTSTRING(";
	   ps a;
	   ps ")"
	 end
      |PRINTINT -> 
	 begin
	   ps "PRINTINT";
	 end	   
      |_ -> failwith "Erreur : Je ne peux afficher cette instruction"
  end;
 
and print_list_instr list_instr = match list_instr with
  |[a] -> print_instr a
  | t::q -> (print_instr t; ps ";" ; prerr_endline ""; print_list_instr q)

let rec print_reg reg = 
  ps "r";
  prerr_int(reg);;



(*
SEQ [
MOVE(STACK(12),CONST(4));
MOVE(STACK(20),CONST(36));
MOVE(STACK(16),CONST(1));
MOVE(TEMP(51),FB);
LABEL(6);
MOVE(TEMP(50),MEM(TEMP(51)));
CJUMP(EQUAL,TEMP(50),CONST(-1),4,5);
LABEL(5);
MOVE(TEMP(49),BINOP(ADD,TEMP(51),CONST(-4)));
MOVE(TEMP(51),MEM(TEMP(49)));
JUMP(6);
LABEL(4);
MOVE(TEMP(48),TEMP(51));
MOVE(TEMP(47),BINOP(ADD,TEMP(48),CONST(-12)));
MOVE(TEMP(46),CONST(0));
MOVE(TEMP(53),MEM(TEMP(47)));
MOVE(TEMP(52),BINOP(ADD,TEMP(47),CONST(-4)));
MOVE(TEMP(54),BINOP(TIMES,TEMP(53),TEMP(46)));
MOVE(TEMP(45),BINOP(SUB,TEMP(52),TEMP(54)));
MOVE(TEMP(44),CONST(2));
MOVE(MEM(TEMP(45)),TEMP(44));
MOVE(TEMP(40),FB);
LABEL(3);
MOVE(TEMP(39),MEM(TEMP(40)));
CJUMP(EQUAL,TEMP(39),CONST(-1),1,2);
LABEL(2);
MOVE(TEMP(38),BINOP(ADD,TEMP(40),CONST(-4)));
MOVE(TEMP(40),MEM(TEMP(38)));
JUMP(3);
LABEL(1);
MOVE(TEMP(37),TEMP(40));
MOVE(TEMP(36),BINOP(ADD,TEMP(37),CONST(-12)));
MOVE(TEMP(35),CONST(0));
MOVE(TEMP(42),MEM(TEMP(36)));
MOVE(TEMP(41),BINOP(ADD,TEMP(36),CONST(-4)));
MOVE(TEMP(43),BINOP(TIMES,TEMP(42),TEMP(35)));
MOVE(TEMP(34),BINOP(SUB,TEMP(41),TEMP(43)));
MOVE(TEMP(33),MEM(TEMP(34)));
MOVE(AA,TEMP(33))
]
*)
