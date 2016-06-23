open Expr
open Symbol_type
open Expr_int
(*pour les permutations aleatoires*)
open Random


let hasard n=
  let places=Array.make (n-1) 0 in 
    for k=0 to (n-2) do places.(k)<- int (k+1) done;
    places 


let rec insere liste valeur rang=
	if rang=0 
	then valeur::liste
	else match liste with
		|[]->[valeur]
		|t::q->t::(insere q valeur (rang-1))

let aleaperm n=
	let hasa=hasard n and debut=ref [1] in 
	for k= 2 to n 
	do debut:=insere !debut k hasa.(k-2) done;
	Array.of_list (!debut)

(*#load "graph.cma"*)

open Graph
open Graph.Pack.Graph
module C = Graph.Coloring.Mark (Graph.Pack.Graph)
let num_color = 16

let rec union r1 r2 = match (r1,r2) with
    a,[]-> a
  |[],a -> a
  |a1::q1, r2 -> 
     if List.mem a1 r2 then union q1 r2
     else a1::(union q1 r2);;

let rec inter r1 r2 = match (r1,r2) with
    a,[]-> a
  |[],a -> []
  |a1::q1, r2 -> 
     if List.mem a1 r2 then inter q1 r2
     else a1::(inter q1 r2);;

let rec one_seq instr = match instr with
    SEQ([])-> []
  | SEQ(a::q)-> (one_seq a)@(one_seq (SEQ(q))) 
  | _ -> [instr];;

let rec use_call templ = match templ with
  | [] -> []
  | (TEMP(r))::q -> r::(use_call q)
  |_-> failwith "que des temps ici"

let use_and_def sequence num_seq  =
  let use = Array.make num_seq [] in
  let def = Array.make num_seq [] in
    for k = 0 to (num_seq -1) do
      begin 
	match sequence.(k) with
	    CJUMP(relop, e1, e2, i1, i2) -> 
	      begin
		match e1, e2 with
		  | TEMP(r1), TEMP(r2) -> use.(k) <- [r1;r2]
		  | MEM(TEMP(r1)), TEMP(r2) -> use.(k) <- [r1;r2]
		  | TEMP(r1), MEM(TEMP(r2)) -> use.(k) <- [r1;r2]
		  | MEM(TEMP(r1)), MEM(TEMP(r2)) -> use.(k) <- [r1;r2]
		  | TEMP(r1), a -> use.(k) <- [r1]
		  | MEM(TEMP(r1)), a -> use.(k) <- [r1]   
		  | a, TEMP(r1) -> use.(k) <- [r1]
		  | a, MEM(TEMP(r1)) -> use.(k) <- [r1] 
		  | _, _ -> failwith "temp ou mem et c'est tout normalement1 "
	      end
	  |MOVE(e1, e2) -> 
	     begin
	       match e1, e2 with
		   TEMP(r1), TEMP(r2) -> (def.(k) <- [r1];use.(k)<- [r2])
		 | MEM(TEMP(r1)), TEMP(r2) -> use.(k) <- [r1;r2]
		 | TEMP(r1), MEM(TEMP(r2)) -> (def.(k) <- [r1];use.(k)<- [r2])
		 | MEM(TEMP(r1)), MEM(TEMP(r2)) -> use.(k) <- [r1;r2]
		 | TEMP(r1), CALL(f,templ) -> (def.(k) <- [r1];use.(k) <- use_call templ)
		 | MEM(TEMP(r1)), CALL(f,templ) -> use.(k) <- (r1::(use_call templ))
		 | TEMP(r2), BINOP(binop,e3,e4) -> 
		     def.(k)<- [r2];
		     begin
		       match e3,e4 with
			   TEMP(r3), TEMP(r4) -> use.(k) <- [r3;r4]
			 | MEM(TEMP(r3)), TEMP(r4) -> use.(k) <- [r3;r4]
			 | TEMP(r3), MEM(TEMP(r4)) -> use.(k) <- [r3;r4]
			 | MEM(TEMP(r3)), MEM(TEMP(r4)) -> use.(k) <- [r3;r4]
			 | TEMP(r3), CONST(n) -> use.(k) <- [r3]
			 | MEM(TEMP(r3)), CONST(n) -> use.(k) <- [r3]   
			 | CONST(n), TEMP(r3) -> use.(k) <- [r3]
			 | CONST(n), MEM(TEMP(r3)) -> use.(k) <- [r3] 
			 | _,_ ->  ()
		     end
		 | TEMP(r1), a -> def.(k) <- [r1]
		 | MEM(TEMP(r1)), a -> use.(k) <- [r1]
		 | a, TEMP(r2) -> use.(k) <- [r2]
		 | a, MEM(TEMP(r2)) ->  use.(k) <- [r2] 
		 | a, BINOP(binop,e3,e4) -> 
		     begin
		       match e3,e4 with
			   TEMP(r3), TEMP(r4) -> use.(k) <- [r3;r4]
			 | MEM(TEMP(r3)), TEMP(r4) -> use.(k) <- [r3;r4]
			 | TEMP(r3), MEM(TEMP(r4)) -> use.(k) <- [r3;r4]
			 | MEM(TEMP(r3)), MEM(TEMP(r4)) -> use.(k) <- [r3;r4]
			 | TEMP(r3), CONST(n) -> use.(k) <- [r3]
			 | MEM(TEMP(r3)), CONST(n) -> use.(k) <- [r3]   
			 | CONST(n), TEMP(r3) -> use.(k) <- [r3]
			 | CONST(n), MEM(TEMP(r3)) -> use.(k) <- [r3] 
			 | _,_ ->  ()
		     end
		 | _,_ -> ()
	     end
	  |_ -> ()
      end
    done;
    use, def 
      
let successeur sequence num_seq =
  let label_use =  Array.make num_seq [] in
  let succ =  Array.make num_seq [] in 
  let max = ref 0 in
    for i = 0 to (num_seq -1) do
      begin 
	match sequence.(i) with
	  |LABEL(k) -> if k > !max then max := k
	  |_ -> ()
      end
    done;
    let label_def = Array.make (!max+1) (-1) in
      for i = 0 to (num_seq -1) do
	begin 
	  match sequence.(i) with
	      CJUMP(relop, e1, e2, i1, i2) -> 
		begin
		  label_use.(i)<- [i1;i2]
		end
	    |JUMP(e) -> 
	       begin
		 label_use.(i)<- [e]
	       end
	    |LABEL(k) ->
	       begin
		 label_def.(k)<- i
	       end
	    |_ -> ()
	end
      done;
      for i = 0 to (num_seq - 1) do
	match label_use.(i) with
	    []-> 
	    if i = num_seq - 1 then succ.(i) <- []
	    else succ.(i) <- [i+1]
	  |[a]-> 
	     if label_def.(a)= (-1) then failwith "ce label n'est pas defini"
	     else succ.(i) <- [label_def.(a)]
	  |[a;b] -> 
	      if label_def.(a)= (-1) or  label_def.(b)=0 then failwith "un des labels n'est pas defini"
	      else succ.(i) <- [label_def.(a);label_def.(b)]
	  |_ -> failwith "liste de plus"
      done;
      succ;;

(*let predecesseur succ num_seq =
  let pred = Array.make num_seq [] in
    for i = num_seq-1 downto 0 do
      for j = 0 to num_seq -1 do
	match succ.(j) with
	  |[] -> ()
	  |[a] -> if a = i then pred.(i) <- j::(pred.(i))
	  |[a;b] -> 
	     if a = i then pred.(i) <- j::(pred.(i))
	     else if b = i then pred.(i) <- j::(pred.(i))
	  |_ -> failwith "c'est quoi ce bordel !!!!!!!!"
      done;
    done;
    pred;;
*)
      

let refresh_out succ inn num_seq = 
  let refresh =  Array.make num_seq [] in
    for i = 0 to num_seq-1 do
      match succ.(i) with
	  []-> ()
	|[a]-> refresh.(i)<- inn.(a)
	|[a;b] -> refresh.(i) <- union inn.(a) inn.(b)
	|_-> ()
    done;
    refresh;;

let refresh_in use out def num_seq = 
  let refresh = use in
    for i = 0 to num_seq-1  do
      refresh.(i) <- union (refresh.(i)) (inter out.(i) def.(i)) 
    done;
    refresh;;

let rec addedges g list sommet = match list with
    [] -> ()
  | [a] -> ()
  | t::q -> (aux g t q sommet; addedges g q sommet)
and aux g e list sommet = match list with
    [] -> ()
  | a::q -> (if a <> e then add_edge g (sommet.(e)) (sommet.(a)); aux g e q sommet);;

let rec maximum liste = match liste with
    [TEMP(r)]-> r
  | TEMP(r)::q -> max r (maximum q)

let maxi sequence num_seq = 
  let max =  ref 0 in
    for i = 0 to num_seq-1 do
      match sequence.(i) with
	| MOVE(TEMP(r),CALL(f,l_temp))-> 
	      let a = maximum (TEMP(r)::l_temp) in
		if a > !max then max:=a
	| MOVE(TEMP(r),_) -> if r > !max then max := r 
	| _ -> ()
    done;
    !max+1;;

let graph sequence num_seq =
  let num_seq = Array.length sequence in
  let use, def = use_and_def sequence num_seq in
  let num_temp = maxi sequence num_seq in
  let in_new = ref use in
  let in_old = ref use in 
  let out_new = ref use in
  let out_old = ref use in
  let succ = successeur sequence num_seq in
  (*let pred = predecesseur succ num_seq in*)
  let g = Graph.Pack.Graph.create () in
  let sommet = Array.make num_temp (V.create (-1)) in
  let color = Array.make num_temp (-1) in
  let max_color = ref (-1) in
    for i = 0 to (num_temp-1) do
      sommet.(i)<- (V.create i);
      add_vertex g (sommet.(i));
    done;
    let flag = ref true in
      out_new := refresh_out succ !in_new num_seq;
      while (!flag) do
	in_old:= !in_new;
	out_old := !out_new;
	in_new := refresh_in use !out_old def num_seq;
	out_new := refresh_out succ !in_new num_seq;
	for i = 0 to num_seq -1 do
	  !in_new.(i) <- List.sort compare (!in_new.(i));
	  !out_new.(i)<- List.sort compare (!out_new.(i))
	done;
	if (!in_new = !in_old) then if !out_new = !out_old then flag:=false;
	(*for i = 0 to num_seq -1 do
	  let rec aux list =
	  match (list) with
	    |[] -> ()
	    |t::q -> (print_int(t);print_string(" "); aux q)
	  in aux (!in_new.(i));
	    print_string(";")
	done;*)
      done;
    for i = 0 to num_seq -1 do
      addedges g !in_new.(i) sommet
    done;
    C.coloring g 100;
    for i = 0 to num_temp-1 do
      color.(i) <- Mark.get (sommet.(i));
      if color.(i)> (!max_color) then max_color:=color.(i)
    done;
    color, (!max_color);;

(*let sequence = Array.of_list (one_seq test2) in
let num_seq = Array.length sequence in
  graph sequence num_seq;;*)

let rec genere descr_func sequence regstack = 
  let res = ref [] in
    for i = 0 to Array.length (sequence) -1 do
      let ok = match sequence.(i) with
	| CJUMP(relop, e1, e2, i1, i2) -> 
	    begin
	      match e1, e2 with
		| TEMP(r1), TEMP(r2) -> CJUMP(relop, regstack.(r1), regstack.(r2),i1,i2)
		| MEM(TEMP(r1)), TEMP(r2) ->  CJUMP(relop, MEM(regstack.(r1)), regstack.(r2),i1,i2)
		| TEMP(r1), MEM(TEMP(r2)) ->  CJUMP(relop, regstack.(r1), MEM(regstack.(r2)),i1,i2)
		| MEM(TEMP(r1)), MEM(TEMP(r2)) ->  CJUMP(relop, MEM(regstack.(r1)), MEM(regstack.(r2)),i1,i2)
		| TEMP(r1), CONST(n) ->  CJUMP(relop, regstack.(r1), CONST(n),i1,i2)
		| MEM(TEMP(r1)), CONST(n) ->  CJUMP(relop, MEM(regstack.(r1)), CONST(n),i1,i2)
		| CONST(n), TEMP(r1) ->  CJUMP(relop, CONST(n), regstack.(r1),i1,i2)
		| CONST(n), MEM(TEMP(r1)) ->  CJUMP(relop, CONST(n), MEM(regstack.(r1)),i1,i2)
		| a,b -> CJUMP(relop, a, b, i1, i2)  (*failwith "temp ou mem et c'est tout normalement8 "*)
	    end
	| MOVE(e1, e2) -> 
	    begin
	      match e1, e2 with
		  a,b when a=b -> NOP
		| TEMP(r1), TEMP(r2) ->   MOVE(regstack.(r1),regstack.(r2))
		| MEM(TEMP(r1)), TEMP(r2) -> MOVE(MEM(regstack.(r1)),regstack.(r2))
		| TEMP(r1), MEM(TEMP(r2)) ->  MOVE(regstack.(r1),MEM(regstack.(r2)))
		| MEM(TEMP(r1)), MEM(TEMP(r2)) ->   MOVE(MEM(regstack.(r1)),MEM(regstack.(r2)))
		| TEMP(r1), CONST(n) ->  MOVE(regstack.(r1),CONST(n))
		| MEM(TEMP(r1)), CONST(n) -> MOVE(MEM(regstack.(r1)),CONST(n))
		| TEMP(r1), CALL(f,temp) -> MOVE(regstack.(r1),CALL(f, change_temp temp regstack))
		| TEMP(r2), BINOP(binop,e3,e4) ->
		    begin
		      match e3,e4 with
			  TEMP(r3), TEMP(r4) -> MOVE(regstack.(r2),BINOP(binop,regstack.(r3),regstack.(r4)))
			| MEM(TEMP(r3)), TEMP(r4) ->  MOVE(regstack.(r2),BINOP(binop,MEM(regstack.(r3)),regstack.(r4)))
			| TEMP(r3), MEM(TEMP(r4)) ->  MOVE(regstack.(r2),BINOP(binop,regstack.(r3),MEM(regstack.(r4))))
			| MEM(TEMP(r3)), MEM(TEMP(r4)) ->  MOVE(regstack.(r2),BINOP(binop,MEM(regstack.(r3)),MEM(regstack.(r4))))
			| TEMP(r3), CONST(n) ->  MOVE(regstack.(r2),BINOP(binop,regstack.(r3),CONST(n)))
			| MEM(TEMP(r3)), CONST(n) ->  MOVE(regstack.(r2),BINOP(binop,MEM(regstack.(r3)),CONST(n)))
			| CONST(n), TEMP(r3) ->  MOVE(regstack.(r2),BINOP(binop,CONST(n),regstack.(r3)))
			| CONST(n), MEM(TEMP(r3)) ->  MOVE(regstack.(r2),BINOP(binop,CONST(n),MEM(regstack.(r3))))
			| _,_ -> failwith "temp ou mem et c'est tout normalement 7"
		    end
		| TEMP(r1), a ->  MOVE(regstack.(r1), a)
		| MEM(TEMP(r1)), a -> MOVE(MEM(regstack.(r1)), a)
		| a, TEMP(r2) -> MOVE(a, regstack.(r2))
		| a, MEM(TEMP(r2)) -> MOVE(a, MEM(regstack.(r2)))
		| a, BINOP(binop,e3,e4) -> 
		    begin
		      match e3,e4 with
			  TEMP(r3), TEMP(r4) -> MOVE(a,BINOP(binop,regstack.(r3),regstack.(r4)))
			| MEM(TEMP(r3)), TEMP(r4) ->  MOVE(a,BINOP(binop,MEM(regstack.(r3)),regstack.(r4)))
			| TEMP(r3), MEM(TEMP(r4)) ->  MOVE(a,BINOP(binop,regstack.(r3),MEM(regstack.(r4))))
			| MEM(TEMP(r3)), MEM(TEMP(r4)) ->  MOVE(a,BINOP(binop,MEM(regstack.(r3)),MEM(regstack.(r4))))
			| TEMP(r3), CONST(n) ->  MOVE(a,BINOP(binop,regstack.(r3),CONST(n)))
			| MEM(TEMP(r3)), CONST(n) ->  MOVE(a,BINOP(binop,MEM(regstack.(r3)),CONST(n)))
			| CONST(n), TEMP(r3) ->  MOVE(a,BINOP(binop,CONST(n),regstack.(r3)))
			| CONST(n), MEM(TEMP(r3)) ->  MOVE(a,BINOP(binop,CONST(n),MEM(regstack.(r3))))
			| _,_ -> failwith "temp ou mem et c'est tout normalement 7"
		    end
		| a,b ->  MOVE(a,b)(*failwith "temp ou mem et c'est tout normalement51"*)
	    end
	| autre -> autre
      in 
	res:= !res@[ok];
    done;
    !res
and change_temp list_temp regstack = match list_temp with
    [] -> []
  |TEMP(r1)::queue -> regstack.(r1)::(change_temp queue regstack)
  | _ -> failwith "GROS BUG"
      

let rec reg_allocate descr_func =
  let _ = Random.init (int_of_float (Sys.time()*.10000.)) in
  let Some (instr) = descr_func.body in
  let permut = aleaperm 16 in
    match descr_func.correspondance with
      |None -> 
	 begin
	   let sequence = Array.of_list (one_seq instr) in
	   let num_seq = Array.length sequence in 
	   let num_temp = maxi sequence num_seq in  
	   let regstack = Array.make num_temp (STACK(0)) in
	   let color, maxi = graph sequence num_seq in
	   let offset = descr_func.offset in
	   let list_temp = ref [] in
	     descr_func.offset <- descr_func.offset + max (4*(maxi-num_color))(0);
	     for i = 0 to num_temp-1 do
	       if color.(i)>num_color then (regstack.(i) <- STACK(offset+4*(color.(i)-num_color)))
	       else ((regstack.(i)<- REG(permut.(color.(i))));descr_func.reg_used <- (union [permut.(color.(i))] descr_func.reg_used))
	     done;
	     for i = 0 to num_color-1 do
	       list_temp:= (!list_temp)@[permut.(i)]
	     done;
	     descr_func.correspondance <- Some(regstack);
	     descr_func.body <- Some (SEQ(genere descr_func sequence regstack));
	 end	
      |Some(_) -> ()
	
