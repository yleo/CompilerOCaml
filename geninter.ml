open Expr
open Expr_int
open Symbol_table
open Symbol_type
open Analyse

let rec iter f l = match l with
  | [] -> []
  | t::q -> f t :: (iter f q)

let reg_counter = ref (-1)
let lab_counter = ref (-1)


let lab () =
  lab_counter:= (!lab_counter+1);
  (!lab_counter)

let get_temp () = 
  reg_counter:= (!reg_counter+1);  
  (!reg_counter)  

let get_absolute_name name symbtbl =
  match name with
    |Int(d) -> d.absolute_name_int
    |Array(d) -> d.absolute_name_array
    |Function(d) -> d.absolute_name_fun
       

(***********************************************)
(* Mets dans le temporaire t l'adresse du premier enregistrement d'activation de la fonction name_parent *)

let rec get_parent name_parent t =
  let deb,corps,fin,temp,t2,t3 = lab(),lab(),lab(),get_temp(),get_temp(),get_temp() in
    SEQ [
      MOVE(TEMP(temp),REG 18);
      LABEL(deb);
      MOVE(TEMP(t2),MEM(TEMP(temp)));
      CJUMP(EQUAL,TEMP t2,CONST name_parent,fin,corps);
      LABEL(corps);
      MOVE(TEMP(t3),BINOP(ADD,TEMP(temp),(CONST (-4))));
      MOVE(TEMP(temp),MEM(TEMP(t3)));
      JUMP(deb);
      LABEL(fin);
      MOVE(TEMP(t),TEMP(temp))
    ]

(***********************************************)
(* met dans le temporaire t la valeur de v *)

let rec compile_var_access v t symbtbl = match v with
    Simple(s) ->
      begin 
	let var = find_var symbtbl s in
	  match var with
	    |Int(d) -> 
	       begin 
		 match d.access_int with
		     Some(Temp(i)) -> 
		       if (d.argtype_int <> By_Reference) then
			 MOVE(TEMP(t),TEMP(i))
		       else
			 MOVE(TEMP(t),MEM(TEMP(i)))
		   | Some(Stack(i)) -> 
		       if (d.argtype_int == By_Reference) then
			 begin
			   let tempo = get_temp() in
			   let parent_var = find_var symbtbl (d.location) in match parent_var with
			     |Function(descr_parent) ->
				let c = get_parent (descr_parent.absolute_name_fun) tempo in
				let t1,t2 = get_temp(),get_temp() in
				  SEQ [c;
				       MOVE(TEMP(t1),BINOP(ADD,TEMP(tempo),CONST (-i)));
				       MOVE(TEMP(t2),MEM(TEMP(t1)));
				       MOVE(TEMP(t),MEM(TEMP(t2)))]
			     |_ -> failwith "erreur 1"
			 end
		       else
			 begin
			   let tempo = get_temp() in
			   let parent_var = find_var symbtbl (d.location) in match parent_var with
			     |Function(descr_parent) ->
				let c = get_parent (descr_parent.absolute_name_fun) tempo in
				let t1 = get_temp() in
				  SEQ [c;
				       MOVE(TEMP(t1),BINOP(ADD,TEMP(tempo),CONST (-i)));
				       MOVE(TEMP(t),MEM(TEMP(t1)))]
			     |_-> failwith "erreur 2"
			 end
		   |None-> failwith ("erreur 456 : "^s^" n'esp pas localise")
	       end
	    |Array(d) -> 
	       begin
		 match d.stack with Some(i) ->
		   if d.argtype_array = Local_Var then
		     begin
		       let tempo = get_temp() in
			   let parent_var = find_var symbtbl (d.location_array) in match parent_var with
			     |Function(descr_parent) ->
				let c = get_parent (descr_parent.absolute_name_fun) tempo in
				let t2 = get_temp() in
				  SEQ [c;
				       MOVE(TEMP(t2),BINOP(ADD,TEMP(tempo),CONST (-i)));
				       MOVE(TEMP(t),MEM(TEMP(t2)))]
			     |_-> failwith "erreur 4"
		     end
		   else (* if by_value... *)
		     begin
		       let tempo = get_temp() in
			 match d.access_array with
			     Some (Temp t2) ->
			       MOVE(TEMP t , MEM(TEMP t2)) 
			   | Some (Stack t2) ->
			       MOVE(TEMP t , MEM(STACK t2)) 
		     end
		   |_-> failwith "erreur 8"
	       end
	    |Function(d) -> 
	       begin 
		 match d.access_func with
		     Some(Temp(i)) -> 
		       MOVE(TEMP(t),TEMP(i))
		   | Some(Stack(i)) -> 
		       begin
			 let tempo = get_temp() in
			 let c = get_parent (d.absolute_name_fun) tempo in
			 let t2 = get_temp() in
			   SEQ [c;
				MOVE(TEMP(t2),BINOP(ADD,TEMP(tempo),CONST (-i)));
				MOVE(TEMP(t),MEM(TEMP(t2)))]
		       end
		   |_-> failwith "erreur 3"
	       end
      end
  | Multiple(v,e) as tab ->
      let t1 = get_temp() in
      let c1 = get_address_ref tab t1 symbtbl in
	SEQ [c1;MOVE(TEMP(t),MEM(TEMP(t1)))]

(***********************************************)
(* mets dans le temporaire t l'adresse de v    *)
     
and get_address_ref v t symbtbl = match v with
    Simple(s) -> 
      begin
	let var = find_var symbtbl s in
	  match var with
	      Int(d) ->
		begin 
		  match d.access_int with
		    |Some(Stack(i)) ->
		       begin
			 let tempo = get_temp() in
			     let parent_var = find_var symbtbl (d.location) in match parent_var with
			       |Function(descr_parent) ->
				  let c = get_parent (descr_parent.absolute_name_fun) tempo in
				    SEQ [c;MOVE(TEMP(t),BINOP(ADD,TEMP(tempo),CONST (-i)))]
			       | _-> failwith "erreur 9"
		       end
		    |Some(Temp(t2))->
		       MOVE(TEMP(t),TEMP(t2))
		    |_ -> failwith "pourquoi il y a une nonee ?"
		end
	    |Array(d) -> 
	       begin
		 if d.argtype_array = Local_Var then
		   match d.stack with Some(i) ->
		     begin
		       let tempo = get_temp() in
		       let parent_var = find_var symbtbl (d.location_array) in match parent_var with
			 |Function(descr_parent) ->
			    let c = get_parent (descr_parent.absolute_name_fun) tempo in
			      SEQ [c;MOVE(TEMP(t),BINOP(ADD,TEMP(tempo),CONST (-i)))]
			 |_-> failwith "erreur 2"
		     end
		     |_-> failwith "erreur 11"
		 else (* if by_value... *)
		   begin
		     let tempo = get_temp() in
		       match d.access_array with
			   Some (Temp t2) ->
			     MOVE(TEMP t , TEMP t2) 
			 | Some (Stack t2) ->
			     MOVE(TEMP t , STACK t2) 
		   end
	       end
	    |Function(d)-> 
	       begin 
		 match d.access_func with
		   |Some(Stack(i)) ->
		      begin
			let tempo = get_temp() in
			let c = get_parent (d.absolute_name_fun) tempo in
			  SEQ [c;MOVE(TEMP(t),BINOP(ADD,TEMP(tempo),CONST (-i)))]
		      end
		   |_-> failwith "erreur 10"
	       end
      end
  | Multiple(v,e) -> 
      let t1,t2 = get_temp(),get_temp() in
      let c1 = get_address_ref v t1 symbtbl in
      let c2 = compile_expr e t2 symbtbl in
      let t3,t4,t5 = get_temp(),get_temp(),get_temp() in
	SEQ [c1;
	     c2;
	     MOVE(TEMP(t4), MEM(TEMP(t1)));
	     MOVE(TEMP(t5),BINOP(ADD,TEMP(t1),CONST (-4)));
	     MOVE(TEMP(t3),BINOP(TIMES,TEMP(t4),TEMP(t2)));
	     MOVE(TEMP(t),BINOP(SUB,TEMP(t5),TEMP(t3)))]

(***********************************************)
(* mets dans le temporaire t la valeur de e    *)

and compile_expr e t symbtbl = match e with
    Expr.Int n -> MOVE(TEMP t,CONST n)
  | Add(a1,a2) -> 
      let t1,t2 = get_temp(),get_temp() in
      let c1 = compile_expr a1 t1 symbtbl and
	  c2 = compile_expr a2 t2 symbtbl in
	SEQ [c1;c2;MOVE(TEMP(t),BINOP(ADD,TEMP(t1),TEMP(t2)))]
  | Sub(a1,a2) -> 
      let t1,t2 = get_temp(),get_temp() in
      let c1 = compile_expr a1 t1 symbtbl and
	  c2 = compile_expr a2 t2 symbtbl in
	SEQ [c1;c2;MOVE(TEMP(t),BINOP(SUB,TEMP(t1),TEMP(t2)))]
  | Times(a1,a2) -> 
      let t1,t2 = get_temp(),get_temp() in
      let c1 = compile_expr a1 t1 symbtbl and
	  c2 = compile_expr a2 t2 symbtbl in
	SEQ [c1;c2;MOVE(TEMP(t),BINOP(TIMES,TEMP(t1),TEMP(t2)))]
  | Div(a1,a2) -> 
      let t1,t2 = get_temp(),get_temp() in
      let c1 = compile_expr a1 t1 symbtbl and
	  c2 = compile_expr a2 t2 symbtbl in
	SEQ [c1;c2;MOVE(TEMP(t),BINOP(DIV,TEMP(t1),TEMP(t2)))]
  | Var(v) -> compile_var_access v t symbtbl
  | App(f,arithl) -> 
      let var = find_var symbtbl f in
	match var with
	  |Function(d) -> 
	     let c,ltemp = compile_app_function f arithl symbtbl (d.parent) in
	       SEQ [c;MOVE(TEMP(t),CALL(f,ltemp))]
	  |_ -> failwith "erreur 666"

(************************************************)
(* rends une liste d'instruction à executer et  *)
(* une liste de temporaires tel qu'en executant *)
(* les instructions, les arguments de f qui     *)
(* sont arithl soit dans la liste de temporaires*)

and compile_app_function f arithl symbtbl symbtbl_arg =
  let rec (boucle: Expr.arg list -> Expr.arithm list -> Expr_int.instr list * Expr_int.exp list) = fun larg -> fun larithm -> match larg,larithm with
    |[],[] -> [],[]
    |arg::q1,arithm::q2 ->
       begin
	 match arg with
	     Val(s,Inttype) ->
	       begin
		 let temp = get_temp() in
		 let res = boucle q1 q2 in
		   (compile_expr arithm temp symbtbl)::(fst res), TEMP(temp)::(snd res)
	       end
	   | Val(s,Arraytype(t,(i,j))) ->
	       begin
		 let temp = get_temp() in
		 let res = boucle q1 q2 in
		 let Var v = arithm in 
		   (get_address_ref v temp symbtbl)::(fst res), TEMP(temp)::(snd res)
	       end
	   | Ref(s) -> 
	       begin
		 let temp = get_temp() in
		   match arithm with
		       Var v -> 
			 let c = get_address_ref v temp symbtbl in
			 let res = boucle q1 q2 in
			   c::(fst res), TEMP(temp)::(snd res)
	       end
       end
    |_ -> failwith "l'analyse statique est fausse" in 
    match find_var symbtbl f with
      |Function(d) ->
	 let petita,petitb = boucle (d.arg) arithl in
	 SEQ (petita) , petitb
      |_-> failwith "erreur 18"
	   
(***********************************************)
(* Initialise un tableau defini explicitement  *)

let rec init_vector v place =
  match v with
    |Vector(v1) -> 
       let c,i=init_vector (v1.(0)) 0 in
       let l = ref [] in
	 for j=0 to Array.length v1 - 1 do
	   l:= (fst(init_vector v1.(j) (place+i*j+4)))::(!l)
	 done;
	 SEQ (MOVE(STACK(place),CONST i)::(!l)) , (4+Array.length v1 * i)
    |Int_sup(Expr.Int(k)) -> 
       MOVE(STACK(place),CONST k), 4
    |_-> failwith "erreur 19"

(***********************************************)
(* Initialise un tableau non defini explicitement *)

let rec init_vector_zero ldim temp_addr_debut =
  match ldim with
      [] -> SEQ[],4
    | (i,j)::q ->
	let n = j-i+1 in
	let t = get_temp() in
	let i = get_temp() in
	let label_debut, label_corps, label_fin = lab(), lab(), lab() in
	let c,taille= init_vector_zero q t in
	  SEQ[MOVE(TEMP i, CONST 0);
	      MOVE(TEMP t,TEMP temp_addr_debut);
	      MOVE(MEM(TEMP t), CONST taille);
	      MOVE(TEMP t, BINOP(ADD, TEMP t, CONST (-4)));
	      LABEL(label_debut);
	      CJUMP(EQUAL,TEMP i,CONST n,label_fin,label_corps);
	      LABEL(label_corps);
	      c;
	      MOVE(TEMP i, BINOP(ADD, TEMP i , CONST (1)));
	      MOVE(TEMP t, BINOP(ADD,TEMP t, CONST (-taille)));
	      JUMP(label_debut);
	      LABEL(label_fin)
	     ],n*taille+4
      
(***********************************************)
(* Decide d'ou mettre quelque chose (dans la pile, *)
(* dans un temporaire... ), et l'y mets...     *)
	  
let rec compile_def def symbtbl =
  let rec compile_def_aux def =
    match def with
      | DefVal(name,t,None) ->
	  begin
	    let var = find_var symbtbl name in
	      match var with
		  Symbol_type.Int(d) -> 
		    if ( d.used_by_descendance) then
		      begin
			let parent = d.location in
			let parent_var = find_var symbtbl parent in
			  match parent_var with
			    |Function(descr) -> 
			       begin
				 d.access_int <-Some(Stack(descr.offset));
				 descr.offset <- descr.offset+4;
				 MOVE(STACK(descr.offset-4),CONST 0)
			       end
			    | _ -> failwith "parent n'est pas une fonction o_O ?"
		      end
		    else
		      begin
			let t = get_temp () in
			  d.access_int <- Some(Temp(t));
			  MOVE(TEMP(t),CONST 0)
		      end
		| Symbol_type.Array(d) -> 
		    begin
		      let parent = d.location_array in
		      let parent_var = find_var symbtbl parent in
			match parent_var with
			  |Function(descr) -> 
			     begin
			       d.stack<-Some(descr.offset);
			       let untemp = get_temp() in
			       let code,taille = (init_vector_zero d.dimension untemp) in
				 descr.offset <- descr.offset + taille;
				 SEQ[MOVE(TEMP untemp,REG 18);MOVE(TEMP untemp,BINOP(ADD,TEMP untemp,CONST (-(descr.offset-taille))));code]
			     end
			  | _ -> failwith "parent n'est pas une fonction o_O ?"
		    end
		|_-> failwith "erreur 21"
	  end
      | DefVal(name,t,Some(Int_sup(n))) ->
	  begin
	    let var = find_var symbtbl name in
	      match var with
		  Int(d) -> 
		    if (d.used_by_descendance) then
		      begin
			let parent = d.location in
			let parent_var = find_var symbtbl parent in
			  match parent_var with
			    |Function(descr) -> 
			       begin
				 d.access_int <-Some(Stack(descr.offset));
				 descr.offset <- descr.offset+4;
				 let temp = get_temp() in
				   SEQ [compile_expr n temp symbtbl;MOVE(STACK(descr.offset-4),TEMP(temp))]
			       end
			    | _ -> failwith "parent n'est pas une fonction o_O ?"
		      end
		    else
		      begin
			let t = get_temp () in
			  d.access_int <- Some(Temp(t));
			  compile_expr n t symbtbl
		      end
		| _ -> failwith "l'analyse statique a laisse passer un entier pour un tableau"
	  end
      | DefVal(name,t,Some(Vector(v))) ->
	  begin
	    let var = find_var symbtbl name in
	      match var with
		| Array(d) -> 
		    begin
		      let parent = d.location_array in
		      let parent_var = find_var symbtbl parent in
			match parent_var with
			  |Function(descr) -> 
			     begin
			       d.stack <- Some(descr.offset);
			       let code,taille = init_vector (Vector(v)) descr.offset in
				 descr.offset <- descr.offset + taille;
				 code
			     end
			  | _ -> failwith "parent n'est pas une fonction o_O ?"
		    end
		| _ -> failwith "l'analstat a mal rempli la table des symboles" 
	  end
      | DefFunc(f,argl,defl,cmd) -> 
	  begin
	    let var = find_var symbtbl f in
	      match var with
		|Function(d) -> 
		   begin
		     if (d.used_by_desc_fun) then
		       begin
			 d.access_func <-Some(Stack(d.offset));
			 d.offset <- d.offset+4
		       end
		     else
		       let t = get_temp () in
			 d.access_func <- Some(Temp(t))
		   end;
		    let _ = iter (fun arg -> compile_arg arg (d.parent)) argl in (* juste pour augmenter l'offset de la fonction ! *)
		      let c1 = (iter (fun def -> compile_def def (d.parent)) (List.rev defl)) in
		      d.body <- Some(SEQ (c1 @ [compile_cmd cmd (d.parent)]));
		      SEQ []
		|_-> failwith "erreur 22"
	  end
  in
    compile_def_aux def

(*******************************************)
(* compile_def pour les arguments          *)

and compile_arg arg symbtbl = match arg with
  | Val(s,Inttype) -> let _ = compile_def (DefVal(s,Inttype,None)) symbtbl in ()
  | Val(s,t) -> 
      let Array descr = find_var symbtbl s in
      let temp = get_temp() in
	descr.access_array <- Some (Temp(temp))
  | Ref(s) -> let _ = compile_def (DefVal(s,Inttype,None)) symbtbl in ()

(*******************************************)
(* transforme une commande pascal en commande intermediaire *)

and compile_cmd cmd symbtbl = match cmd with
    Seq(cmd_list) -> 
	SEQ(iter (fun cmd -> compile_cmd cmd symbtbl) cmd_list)
  | Ifte(cond,c1,c2) ->
      let lab1,lab2,lab3 = lab(),lab(),lab() in
      let c = compile_cond cond lab1 lab2 symbtbl in
	SEQ [
	  c;
	  LABEL(lab1);
	  compile_cmd c1 symbtbl;
	  JUMP(lab3);
	  LABEL(lab2);
	  compile_cmd c2 symbtbl;
	  LABEL(lab3)
	]
  | While(cond,cmd) ->
      let lab0,lab1,lab2 = lab(),lab(),lab() in
      let c = compile_cond cond lab1 lab2 symbtbl in
	SEQ [
	  LABEL(lab0);
	  c;
	  LABEL(lab1);
	  compile_cmd cmd symbtbl;
	  JUMP(lab0);
	  LABEL(lab2)
	]
  | For(v,e1,e2,cmd) ->
      SEQ [
	compile_cmd (Assign(v,e1)) symbtbl;
	compile_cmd (While(Op(Neq,Add(Var v,Expr.Int (-1)),e2),Seq([cmd;Assign(v,Add(Var v,Expr.Int 1))]))) symbtbl
      ]
  | Print(printable) ->
      begin
	match printable with
	    PString(s) -> PRINTSTRING(s)
	  | PInt(arithm) ->  let temp = get_temp() in 
	    let c =compile_expr arithm temp symbtbl in
	      SEQ[c;MOVE(REG 17,TEMP(temp)); PRINTINT]
      end
  | Assign(var,e1) -> 
      begin
	match var with
	  |Simple(s) ->
	     begin
	       let rien = find_var symbtbl s in match rien with
		 |Int(d) -> 
		    begin
		      match d.access_int,d.argtype_int with
			| Some(Stack(i)),By_Reference -> 
			    let temp1,temp2 = get_temp(),get_temp() in
			    let c1 = compile_var_access var temp1 symbtbl in
			    let c2 = compile_expr e1 temp2 symbtbl in
			      SEQ [c1;c2;MOVE(MEM(TEMP(temp1)),TEMP(temp2))] 
			| Some(Temp(t)),By_Reference ->
			    let temp = get_temp() in
			    let c = compile_expr e1 temp symbtbl in
			      SEQ [c;MOVE(MEM (TEMP(t)),TEMP(temp))]
			| Some(Stack(i)),_ -> 
			    let temp1,temp2 = get_temp(),get_temp() in
			    let c1 = compile_var_access var temp1 symbtbl in
			    let c2 = compile_expr e1 temp2 symbtbl in
			      SEQ [c1;c2;MOVE(TEMP(temp1),TEMP(temp2))] 
			| Some(Temp(t)),_ ->
			    let temp = get_temp() in
			    let c = compile_expr e1 temp symbtbl in
			      SEQ [c;MOVE(TEMP(t),TEMP(temp))]
			| None,_ -> failwith ("erreur 23 : "^s)
		    end
		 |Array(d) -> 
		    let temp1,temp2 = get_temp(),get_temp() in
		    let c1 = compile_var_access var temp1 symbtbl in
		    let c2 = compile_expr e1 temp2 symbtbl in
		      SEQ [c1;c2;MOVE(MEM(TEMP(temp1)),TEMP(temp2))] 
		 |Function d -> 
		    begin
		      match d.access_func with
			| Some(Stack(i)) -> 
			    let temp1,temp2 = get_temp(),get_temp() in
			    let c1 = get_address_ref var temp1 symbtbl in
			    let c2 = compile_expr e1 temp2 symbtbl in
			      SEQ [c1;c2;MOVE(MEM(TEMP(temp1)),TEMP(temp2))] 
			| Some(Temp(t)) ->
			    let temp = get_temp() in
			    let c = compile_expr e1 temp symbtbl in
			      SEQ [c;MOVE(TEMP(t),TEMP(temp))]
			| None -> failwith ("erreur 23 : "^s)
		    end
	     end
	  |Multiple(s,e) -> 
	     begin 
	       let temp1,temp2 = get_temp(),get_temp() in
	       let c1 = get_address_ref var temp1 symbtbl in
	       let c2 = compile_expr e1 temp2 symbtbl in
		 SEQ [c1;c2;MOVE(MEM(TEMP(temp1)),TEMP(temp2))] 
	     end
      end
		      
(*******************************************)
(* compile toute ces fonction de compilation ! *)

and compile_inter prgm symbtbl = 
  let defs,cmd = prgm in
  let c1 = iter (fun def -> compile_def def symbtbl) (List.rev defs) in
  let c2 = compile_cmd cmd symbtbl in
    SEQ (c1@[c2])

(*******************************************)
(* compile les conditions à base de labels pour les and, or, etc. *)
(* pas forcement efficace (bcp de jump) mais ne doit pas changer *)
(* grand chose en pratique !               *)

and compile_cond cond lab1 lab2 symbtbl =
  match cond with
      Or(c1,c2) -> 
	let label = lab() in
	  SEQ [
	    compile_cond c1 lab1 label symbtbl;
	    LABEL(label);
	    compile_cond c2 lab1 lab2 symbtbl]
    | And(c1,c2) ->
	let label = lab() in
	  SEQ [
	    compile_cond c1 label lab2 symbtbl;
	    LABEL(label);
	    compile_cond c2 lab1 lab2 symbtbl]
    | Xor(c1,c2) -> 
	let label1,label2 = lab(),lab() in
	  SEQ [
	    compile_cond c1 label1 label2 symbtbl;
	    LABEL(label1);	 
	    compile_cond c2 lab2 lab1 symbtbl;
	    LABEL(label2);
	    compile_cond c2 lab1 lab2 symbtbl]
    | Not(c) -> compile_cond c lab2 lab1 symbtbl
    | Op(op,e1,e2)-> 
	let t1,t2 = get_temp(),get_temp() in
	let c1,c2 = compile_expr e1 t1 symbtbl, compile_expr e2 t2 symbtbl in
	  SEQ [c1;c2;CJUMP(compile_op op, TEMP(t1), TEMP(t2), lab1, lab2)] 

and compile_op op = match op with
    Equal -> EQUAL
  | Neq -> NEQ
  | Leq -> LEQ
  | Lneq -> LNEQ
  | Geq -> GEQ
  | Gneq -> GNEQ

  
