open Expr
open Expr_int
open Symbol_table
open Symbol_type
open Analyse
open Liveness
open Hashtbl

(*******************************************************************************)
(* Fonctions pour recuperer une liste des descripteurs de fonctions utiles     *)
(*******************************************************************************)


let rec add l s =
  match l with
      [] -> [s]
    | t::q -> if t.absolute_name_fun=s.absolute_name_fun then l else t::(add q s)

let rec merge l1 l2 =
  match l1 with
    [] -> l2
  | t::q -> merge q (add l2 t) 

let rec get_all_func code symbtbl =
  match code with
      SEQ [] -> []
    | SEQ (t::l) ->
	flush stdout;
	merge (get_all_func t symbtbl) (get_all_func (SEQ l) symbtbl)
    | MOVE(e1,CALL(f,l)) -> 

	let Function descr = find_var symbtbl f in 
	let Some body = descr.body in
	  if not descr.printed then
	    begin
	      descr.printed <- true;
	      let l = (get_all_func body descr.parent) in
		add l descr
	    end		
	  else		
	    []
    | _ -> []

(*******************************************************************************)
(* Fonctions utiles pour l'impressions du code                                 *)
(*******************************************************************************)

let translation = [|10;11;12;13;14;15;16;17;18;19;20;21;22;23;24;25;4;30;29|](* ;0;0;0;0;0;0;0;0 |]*)
let all_reg = [1;2;3;4;5;6;7;8;9;10;11;12;13;14;15;16;17](* ;0;0;0;0;0;0;0;0 |]*)

let rec iter1 l f = match l with
    [] -> []
  | t::q -> (f t)::(iter1 q f)

let rec iter l f = match l with
  | [] -> ()
  | t::q -> f t ; iter q f

let ps = print_string
let p s = ps (s^"\n")
let roi i = "$"^(string_of_int (translation.(i-1)))
let loi i = "label"^(string_of_int i)
let soi i = string_of_int i
let foi f = "fonktion"^(soi f)

let rec save_reg l i =
  match l with
      [] -> []
    | r::q ->
	p ("sw "^(roi r)^", "^(soi (-(i)))^"($fp)") (* ici on svg tt les reg *);
	(r,-i)::save_reg q (i+4)

let rec recover_reg l i a =
  match l with
      [] -> ()
    | r::q ->
	if r <> a then
	  p ("lw "^(roi r)^", "^(soi (-(i)))^"($fp)");
	recover_reg q (i+4) a


let rec print_ltemp ltemp = match ltemp with
    [] -> ()
  | REG t1::q -> p ("REG "^(soi t1)); print_ltemp q
  | STACK t1::q -> p ("STACK "^(soi t1)); print_ltemp q

let rec print_larg ltemp = match ltemp with
    [] -> ()
  | Ref s::q -> p ("ref "^(s)); print_larg q
  | Val (s,a)::q -> p ("val "^(s)); print_larg q

let rec get_pile source r = match source with
    [] -> failwith "pasla"
  | (t,p)::q when t=r -> p
  | _::q -> get_pile q r


(*******************************************************************************)
(* Fonctions utiles pour l'enregistrement d'activation                         *)
(*******************************************************************************)

(*   Pour l'interface entre les location d'une fonction depuis une autre   *)

let get_location arg symbtbl fd =
  let res = ref "" in
    begin
      match arg with
	  Ref(s) | Val(s,_) -> 
	    res := s
    end;
    let a = find_var symbtbl (!res) in
      match a with
	  Int(d) ->
	    begin match d.access_int with
		Some(ac) -> 
		  begin match ac with
		      Temp t -> 
			begin
			  match fd.correspondance with
			    |None -> failwith "on a trouve"
			    |Some res -> res.(t)
			end
		    | Stack t -> STACK t
		  end
	    end
	| Array(d) ->
	    begin
	      match d.access_array with
		  Some (Temp t) -> 
		    let Some corr = fd.correspondance in
		      corr.(t)
		| Some (Stack t) -> STACK t
	    end
	|_ -> failwith "cest le progrzamme le caca"

(*   Placer les arguments sans rien ecraser !     *) 

let put_argument ltemp larg fd symbtbl source =
  let rec aux ltemp larg = match ltemp, larg with
      [],[] -> ()
    | a1::q1 , arg::q2 -> 
	begin
	  match a1 with
	      REG(t1) -> 
		begin
		  let pile = get_pile source t1 in
		  match get_location arg symbtbl fd with
		      REG t2 ->   
			p ("lw "^(roi t2)^", "^(soi pile)^"($t1)")
		    | STACK(t2) -> 
			p ("lw $t0, "^(soi pile)^"($t1)");
			p ("sw $t0, -"^(soi (t2))^"($fp)")
		end
	    | STACK(t1) ->
		begin
		  match get_location arg symbtbl fd with
		      REG t2 ->   
			p ("lw "^(roi t2)^" -"^(soi (t1))^"($t1)")
		    | STACK(t2) -> 
			p ("lw $t0 -"^(soi (t1))^"($t1)");
			p ("sw $t0 -"^(soi (t2))^"($fp)")
		end
	    | _ -> failwith "c'est nous les cacas"
	end;
	aux q1 q2 
    |_-> p "sefgdrgyjfydjuj" in
    aux ltemp larg

(*******************************************************************************)
(*    Fonctions pour pouvoir imprimer des chaines de caracteres                *)
(*******************************************************************************)


let i = ref 0 

let string_table = Hashtbl.create 10 ;;
Hashtbl.add string_table "" "" ;;
let rec fill_string descr =
  let Some code = descr.body in
  let rec aux code = match code with
      SEQ l -> (iter l (fun c -> aux c))
    | PRINTSTRING(s) -> 
        let sreturn = (String.sub s 0 (String.length s - 1))^"\n\"" in
	  p ("string"^(soi !i)^": .asciiz "^(sreturn^"\n"));
	  Hashtbl.add string_table s ("string"^(soi !i));
	  i := (!i)+1
    | _ -> () in
    aux code

(******************************************************************************)
(**********      ******    ***** **********************************************)
(********** ********** **** **** **********************************************)
(********** ********** **** **** **********************************************)
(********** ***   **** **** **** **********************************************)
(********** **** ***** **** ***************************************************)
(**********      ******    ***** **********************************************)
(******************************************************************************)
	
let rec compile_spim cmd f_descr = match cmd with
    SEQ(cl) -> iter cl (fun cmd -> compile_spim cmd f_descr)
  | JUMP(l) -> p ("j "^(loi l))
  | MOVE(dest,prov) ->
      ();
      begin match dest with
	| REG(i) ->
	    begin match prov with
	      | CONST j ->
		  p ("li "^(roi i)^", "^(soi j))
	      | REG(j) ->
		  p ("move "^(roi i)^", "^(roi j))                      (* rappel : dest = REG(i)  *)
	      | STACK(j) ->
		  p ("lw "^(roi i)^", -"^(soi j)^"($fp)")
	      | MEM (REG j) -> 
		  p ("lw "^(roi i)^", "^"0("^(roi j)^")")
	      | MEM (STACK j) -> 
		  p ("lw $t0, -"^(soi j)^"($fp)");
		  p ("lw "^(roi i)^", "^"0($t0)")                      (* rappel : dest = REG(i)  *)
	      | BINOP(b,REG(j),REG(k)) ->
		  begin
		    match b with
			ADD ->
			  p ("add "^(roi i)^", "^(roi j)^", "^(roi k))
		      | SUB ->
			  p ("sub "^(roi i)^", "^(roi j)^", "^(roi k))                      (* rappel : dest = REG(i)  *)
		      | TIMES ->
			  p ("mul "^(roi i)^", "^(roi j)^", "^(roi k))
		      | DIV ->
			  p ("beqz "^(roi k)^", div_by_zero");
			  p ("div "^(roi i)^", "^(roi j)^", "^(roi k))
		  end
	      | BINOP(b,REG(j),CONST(k)) ->
		  begin
		    match b with
			ADD ->
			  p ("addi "^(roi i)^", "^(roi j)^", "^(soi k))                      (* rappel : dest = REG(i)  *)
		      | SUB ->
			  p ("addi "^(roi i)^", "^(roi j)^", "^(soi (-k)))
		      | TIMES ->
			  p ("li $t0, "^(soi k));
			  p ("mul "^(roi i)^", "^(roi j)^", $t0")
		      | DIV ->
			  p ("li $t0, "^(soi k));
			  p ("beqz $t0, div_by_zero");
			  p ("div "^(roi i)^", "^(roi j)^", $t0")
		  end
	      | BINOP(b,REG(j),STACK(k)) ->
		  begin
		    match b with
			ADD ->
			  p ("lw $t0, -"^(soi k)^"($fp)");                      (* rappel : dest = REG(i)  *)
			  p ("add "^(roi i)^", "^(roi j)^", $t0")
		      | SUB ->
			  p ("lw $t0, -"^(soi k)^"($fp)");
			  p ("sub "^(roi i)^", "^(roi j)^", $t0")
		      | TIMES ->
			  p ("lw $t0, -"^(soi k)^"($fp)");
			  p ("mul "^(roi i)^", "^(roi j)^", $t0")
		      | DIV ->
			  p ("lw $t0, -"^(soi k)^"($fp)");                      (* rappel : dest = REG(i)  *)
			  p ("beqz $t0, div_by_zero");
			  p ("div "^(roi i)^", "^(roi j)^", $t0")
		  end
	      | BINOP(b,STACK(j),REG(k)) ->
		  begin
		    match b with
			ADD ->
			  p ("lw $t0, -"^(soi j)^"($fp)");
			  p ("add "^(roi i)^", $t0, "^(roi k))
		      | SUB ->
			  p ("lw $t0, -"^(soi j)^"($fp)");
			  p ("sub "^(roi i)^", $t0, "^(roi k))
		      | TIMES ->
			  p ("lw $t0, -"^(soi j)^"($fp)");
			  p ("mul "^(roi i)^", $t0, "^(roi k))                      (* rappel : dest = REG(i)  *)
		      | DIV ->
			  p ("lw $t0, -"^(soi j)^"($fp)");
			  p ("beqz "^(roi k)^", div_by_zero");
			  p ("div "^(roi i)^", $t0, "^(roi k))
		  end
	      | BINOP(b,STACK(j),CONST(k)) ->
		  begin
		    match b with
			ADD ->
			  p ("lw $t0, -"^(soi j)^"($fp)");
			  p ("addi "^(roi i)^", $t0, "^(soi k))
		      | SUB ->
			  p ("lw $t0, -"^(soi j)^"($fp)");
			  p ("addi "^(roi i)^", $t0, "^(soi (-k)))                      (* rappel : dest = REG(i)  *)
		      | TIMES ->
			  p ("lw $t0, -"^(soi j)^"($fp)");
			  p ("li $t1, "^(soi k));
			  p ("mul "^(roi i)^", $t0, $t1")
		      | DIV ->
			  p ("lw $t0, -"^(soi j)^"($fp)");
			  p ("li $t1, "^(soi k));                      (* rappel : dest = REG(i)  *)
			  p ("beqz $t1, div_by_zero");
			  p ("div "^(roi i)^", $t0, $t1")
		  end
	      | BINOP(b,STACK(j),STACK(k)) ->
		  begin
		    match b with
			ADD ->
			  p ("lw $t0, -"^(soi j)^"($fp)");
			  p ("lw $t1, -"^(soi k)^"($fp)");
			  p ("add "^(roi i)^", $t0, $t1")
		      | SUB ->
			  p ("lw $t0, -"^(soi j)^"($fp)");                      (* rappel : dest = REG(i)  *)
			  p ("lw $t1, -"^(soi k)^"($fp)");
			  p ("sub "^(roi i)^", $t0, $t1")
		      | TIMES ->
			  p ("lw $t0, -"^(soi j)^"($fp)");
			  p ("lw $t1, -"^(soi k)^"($fp)");
			  p ("mul "^(roi i)^", $t0, $t1")
		      | DIV ->
			  p ("lw $t0, -"^(soi j)^"($fp)");
			  p ("lw $t1, -"^(soi k)^"($fp)");
			  p ("beqz $t1, div_by_zero");
			  p ("div "^(roi i)^", $t0, $t1")                      (* rappel : dest = REG(i)  *)
		  end
	      | CALL(f,templ) ->
		 
		  let Function f_descr2 = 
		    find_var (f_descr.parent) f in
		   
		  let symbtbl = f_descr2.parent in
		    let l_reg_used = f_descr2.reg_used in
		    let source = save_reg l_reg_used (f_descr.offset) in
		      p ("sw $ra, -8($fp)");
		      p ("move $t1, $fp");
		      p ("addi $fp, $fp, "^(soi (-f_descr.offset-List.length l_reg_used*4)));
		      p ("sw $t1, -4($fp)");
		      put_argument templ f_descr2.arg f_descr2 symbtbl source;
		      p ("li $t0, "^(soi f_descr2.absolute_name_fun));                      (* rappel : dest = REG(i)  *)
		      p ("sw $t0, 0($fp)");
		      p ("jal "^(foi f_descr2.absolute_name_fun));
		      begin
			match f_descr2.access_func with
			    Some (Stack(d)) -> 
			      p ("lw "^(roi i)^", -"^(soi d)^"($fp)") 
			  | Some (Temp d) ->
			      let Some corres = f_descr2.correspondance in 
				match corres.(d) with
				    STACK d -> 
				      p ("lw "^(roi i)^", -"^(soi d)^"($fp)") 
				  | REG d ->
				      p ("move "^(roi i)^", "^(roi d))                       (* rappel : dest = REG(i)  *)
				  |_ -> failwith "cacacaca"
		      end;
		      p ("lw $fp, -4($fp)");
		      p ("lw $ra, -8($fp)");
		      recover_reg l_reg_used (f_descr.offset) i
		      
	    (* 
	       enregistrement d'activation
	       jal ...
	       defaire enregistrement d'activation
	    *)
	    end                              (* fin du match sur dest = REG(i) !!!!!   *)
	| STACK(i) -> 
	    begin match prov with
	      | CONST j ->
		  p ("li $t0, "^(soi j));
		  p ("sw $t0, -"^(soi i)^"($fp)")                      (* rappel : dest = STACK(i)  *)
	      | REG j ->
		  p ("sw "^(roi j)^", -"^(soi i)^"($fp)")
	      | STACK(j) ->
		  p ("lw "^("$t0")^", -"^(soi j)^"($fp)");
		  p ("sw $t0, -"^(soi i)^"($fp)")
	      | MEM (REG j) -> 
		  p ("lw $t0, 0("^(roi j)^")");                      (* rappel : dest = STACK(i)  *)
		  p ("sw $t0, -"^(soi i)^"($fp)")
	      | MEM (STACK j) -> 
		  p ("lw $t0, -"^(soi j)^"($fp)");
		  p ("lw $t0, 0($t0)");
		  p ("sw $t0, -"^(soi i)^"($fp)")		  
	      | BINOP(b,REG(j),REG(k)) ->                      (* rappel : dest = STACK(i)  *)
		  begin
		    match b with
			ADD ->
			  p ("add $t0, "^(roi j)^", "^(roi k))
		      | SUB ->
			  p ("sub $t0, "^(roi j)^", "^(roi k))
		      | TIMES ->
			  p ("mul $t0, "^(roi j)^", "^(roi k))
		      | DIV ->
			  p ("beqz "^(roi k)^", div_by_zero");
			  p ("div $t0, "^(roi j)^", "^(roi k))                      (* rappel : dest = STACK(i)  *)
		  end;
		  p ("sw $t0, -"^(soi i)^"($fp)")
	      | CALL(f,templ) ->
		  
		  let Function f_descr2 = 
		    find_var (f_descr.parent) f in
		    
		  let symbtbl = f_descr2.parent in
		    let l_reg_used = f_descr2.reg_used in
		      save_reg l_reg_used (f_descr.offset);                      (* rappel : dest = STACK(i)  *)
		      p ("sw $ra, -8($fp)");
		      p ("move $t1, $fp");
		      p ("addi $fp, $fp, "^(soi (-f_descr.offset-List.length l_reg_used*4)));
		      p ("sw $t1, -4($fp)");
		      put_argument templ f_descr2.arg f_descr2 symbtbl;
		      p ("li $t0, "^(soi f_descr2.absolute_name_fun));
		      p ("sw $t0, 0($fp)");
		      p ("jal "^(foi f_descr2.absolute_name_fun));
		      begin
			match f_descr2.access_func with
			    Some (Stack(d)) -> 
			      p ("lw "^(roi i)^", -"^(soi d)^"($fp)") 
			  | Some (Temp d) ->
			      let Some corres = f_descr2.correspondance in                       (* rappel : dest = STACK(i)  *)
				match corres.(d) with
				    STACK d -> 
				      p ("lw $t0, -"^(soi d)^"($fp)");
				  | REG d ->
				      p ("move $t0, "^(roi d)) 
				  |_ -> failwith "cacacaca"                      (* rappel : dest = STACK(i)  *)
		      end;
		      p ("lw $fp, -4($fp)");
		      p ("sw $t0, -"^(soi i)^"($fp)");
		      p ("lw $ra, -8($fp)");
		      recover_reg l_reg_used (f_descr.offset) i
			
	    (* 
	       enregistrement d'activation
	       jal ...
	       defaire enregistrement d'activation
	    *)
	    (*		  begin
			  
			  end*)
			
	      | BINOP(b,STACK(j),CONST(k)) ->                      (* rappel : dest = STACK(i)  *)
		  begin
		    match b with
			ADD ->
			  p ("lw $t0, -"^(soi j)^"($fp)");
			  p ("addi $t1, $t0, "^(soi k));                      (* rappel : dest = STACK(i)  *)
			  p ("sw $t1, -"^(soi i)^"($fp)")
		      | SUB ->
			  p ("lw $t0, -"^(soi j)^"($fp)");
			  p ("addi $t1, $t0, -"^(soi k));
			  p ("sw $t1, -"^(soi i)^"($fp)")
		      | TIMES ->
			  p ("lw $t0, "^(soi j)^"($fp)");
			  p ("li $t1, "^(soi k));
			  p ("mul $t1, $t0, $t1");
			  p ("sw $t1, -"^(soi i)^"($fp)")
		      | DIV ->
			  p ("lw $t0, -"^(soi j)^"($fp)");                      (* rappel : dest = STACK(i)  *)
			  p ("li $t1, "^(soi k));
			  p ("beqz $t1, div_by_zero");
			  p ("div $t1, $t0, $t1");
			  p ("sw $t1, -"^(soi i)^"($fp)")
		  end
	      | BINOP(b,REG(j),CONST(k)) ->
		  begin
		    match b with
			ADD ->
			  p ("addi "^(roi i)^", "^(roi j)^", "^(soi k))                      (* rappel : dest = STACK(i)  *)
		      | SUB ->
			  p ("addi "^(roi i)^", "^(roi j)^", -"^(soi k))
		      | TIMES ->
			  p ("li $t0, "^(soi k));                      (* rappel : dest = STACK(i)  *)
			  p ("mul "^(roi i)^", "^(roi j)^", $t0")
		      | DIV ->
			  p ("li $t0, "^(soi k));
			  p ("beqz $t0, div_by_zero");
			  p ("div "^(roi i)^", "^(roi j)^", $t0")                      (* rappel : dest = STACK(i)  *)
		  end
	      | BINOP(b,REG(j),STACK(k)) ->
		  begin
		    match b with
			ADD ->
			  p ("lw $t0, -"^(soi k)^"($fp)");
			  p ("add $t0, "^(roi j)^", $t0")                      (* rappel : dest = STACK(i)  *)
		      | SUB ->
			  p ("lw $t0, -"^(soi k)^"($fp)");
			  p ("sub "^("$t0")^", "^(roi j)^", $t0")
		      | TIMES ->
			  p ("lw $t0, -"^(soi k)^"($fp)");
			  p ("mul "^("$t0")^", "^(roi j)^", $t0")
		      | DIV ->
			  p ("lw $t0, -"^(soi k)^"($fp)");
			  p ("beqz $t0, div_by_zero");
			  p ("div "^("$t0")^", "^(roi j)^", $t0")                      (* rappel : dest = STACK(i)  *)
		  end;
		  p ("sw $t0, -"^(soi i)^"($fp)")
	      | BINOP(b,STACK(j),REG(k)) ->
		  begin
		    match b with
			ADD ->
			  p ("lw $t0, -"^(soi j)^"($fp)");
			  p ("add $t0, $t0, "^(roi k))
		      | SUB ->
			  p ("lw $t0, -"^(soi j)^"($fp)");                      (* rappel : dest = STACK(i)  *)
			  p ("sub $t0, $t0, "^(roi k))
		      | TIMES ->
			  p ("lw $t0, -"^(soi j)^"($fp)");
			  p ("mul "^("$t0")^", $t0, "^(roi k))
		      | DIV ->
			  p ("lw $t0, -"^(soi j)^"($fp)");
			  p ("beqz "^(roi k)^", div_by_zero");
			  p ("div $t0, $t0, "^(roi k))
		  end;
		  p ("sw $t0, -"^(soi i)^"($fp)")
	      | BINOP(b,STACK(j),STACK(k)) ->
		  begin
		    match b with
			ADD ->
			  p ("lw $t0, -"^(soi j)^"($fp)");                      (* rappel : dest = STACK(i)  *)
			  p ("lw $t1, -"^(soi k)^"($fp)");
			  p ("add $t0, $t0, $t1")
		      | SUB ->
			  p ("lw $t0, -"^(soi j)^"($fp)");
			  p ("lw $t1, -"^(soi k)^"($fp)");
			  p ("sub $t0, $t0, $t1")
		      | TIMES ->
			  p ("lw $t0, -"^(soi j)^"($fp)");
			  p ("lw $t1, -"^(soi k)^"($fp)");
			  p ("mul $t0, $t0, $t1")                      (* rappel : dest = STACK(i)  *)
		      | DIV ->
			  p ("lw $t0, -"^(soi j)^"($fp)");
			  p ("lw $t1, -"^(soi k)^"($fp)");
			  p ("beqz $t1, div_by_zero");
			  p ("div $t0, $t0, $t1")
		  end;
		  p ("sw $t0, -"^(soi i)^"($fp)")
	    end                         (* fin du match sur dest = STACK(i) !!!! *)
	| MEM(REG(i)) -> 
	    begin match prov with
	      | CONST j -> 
		  p ("li $t0, "^(soi j));
		  p ("sw $t0, 0("^(roi i)^")")
	      | REG j ->
		  p ("sw "^(roi j)^", 0("^(roi i)^")")
	      | STACK(j) -> 
		  p ("lw $t0, -"^(soi j)^"($fp)");
		  p ("sw $t0, 0("^(roi i)^")")
	    end
	| MEM(STACK(i)) ->
	    begin match prov with
	      | CONST j -> 
		  p ("li $t0, "^(soi j));
		  p ("lw $t1, -"^(soi i)^"($fp)");
		  p ("sw "^(roi j)^", 0("^(roi i)^")")
	      | REG j ->
		  p ("lw $t0, -"^(soi j)^"($fp)");
		  p ("sw "^(roi j)^", 0($t0)")
	      | STACK(j) -> 
		  p ("lw $t1, -"^(soi i)^"($fp)");
		  p ("lw $t0, -"^(soi j)^"($fp)");
		  p ("sw $t0, 0($t1)")		  
	    end
      end
  | LABEL(i) -> p ((loi i)^":")
  | PUSH(REG(i)) ->
      p ("sw "^(roi i)^"0($sp)");
      p "addiu $sp, $sp-4"
  | PRINTSTRING(s) -> 
      p ("li $v0, 4
	la $a0,"^(Hashtbl.find string_table s)^"
	syscall")
  | PRINTINT -> 
      p "li $v0, 1";
      p "syscall";
      p ("li $v0, 4");
      p "la $a0, linefeed";
      p "syscall"
  | CJUMP(relop,e1,e2,lab1,lab2) ->
      let op = ref "" and
	  r1 = ref "" and
	  r2 = ref "" and
	  l1 = loi lab1 and
	  l2 = loi lab2 in
	begin match relop with
	  | EQUAL -> op := "beq "
	  | NEQ -> op := "bne "
	  | LEQ -> op := "ble "
	  | LNEQ -> op := "blt "
	  | GEQ -> op := "bge "
	  | GNEQ -> op := "bgt "
	end;
	begin match e1 with
	  | REG(i) -> r1 := roi i
	  | STACK(i) ->
	      p ("lw $t0, -"^(soi i)^"($fp)");
	      r1 := "$t0"
	  | CONST i -> 
	      p ("li $t0,"^(soi i));
	      r1 := "$t0"
	end;
	begin match e2 with
	  | REG(i) -> r2 := roi i
	  | STACK(i) ->
	      p ("lw $t1, -"^(soi i)^"($fp)");
	      r2 := "$t1"
	  | CONST i -> 
	      p ("li $t1,"^(soi i));
	      r2 := "$t1"
	end;
	p (!op^(!r1)^", "^(!r2)^", "^l1);
	p ("j "^l2)
	  
  | _ ->  failwith "c'est ici"






let compile_func descr = 
  p ((foi descr.absolute_name_fun)^":");
  let Some body = descr.body in
    compile_spim body descr  ;
    p "jr $ra"

let print_descr fdescr =
  prerr_endline (" la fonction est : "^soi fdescr.absolute_name_fun); 
  let Some caca = fdescr.body in
    print_instr caca;
    p ""


let tospim code_inter symbtbl =

  let Function descr = Analyse.find_var symbtbl "program" in
  let Some(code) = descr.body in
  let l_function = (get_all_func code symbtbl) in 
  
    iter l_function Liveness.reg_allocate;
   
(*    iter l_function print_descr;*)
  
    Liveness.reg_allocate descr;
    (*print_descr descr;*)
  
    let Some bodyallocated = descr.body in

      p ".data";
      p "linefeed : .asciiz \"\n\"";
      iter l_function fill_string;
      fill_string descr;
      p "
.text
.globl main

main:

move $fp,$sp
li $t0, -1
sw $t0,0($fp)
";
      compile_spim bodyallocated descr ;
	p "li $v0, 10
syscall";
	
	iter l_function compile_func ;

