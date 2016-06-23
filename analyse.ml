open Expr	
open Symbol_type

let rec verif_type symbtbl t1 t2 =
  match t1,t2 with
    |Arraytype(a1,_),Arraytype(a2,_)-> verif_type symbtbl a1 a2
    |Inttype, Inttype -> ()
    |_,_ -> failwith "le tableau n'a pas le bon type"
     


(***********************************************)
(* Fonction utilisee tout au long du compilateur pour acceder *)
(* au descripteur d'une variable               *)

let rec find_var symbtbl name = 
  match symbtbl with
      Root(htbl) -> 
	begin
 	  try Hashtbl.find htbl name
	  with
	    | Not_found -> failwith (name^" n'est pas defini")
	end
    | Locale(previous,htbl) ->
	begin
 	  try Hashtbl.find htbl name
	  with
	    | Not_found -> let var = find_var previous name in
		match var with
		  |Int(d) -> 
		     begin
		       d.used_by_descendance <- true ; 
		       var 
		     end
		  |Function(d) -> 
		     begin
		       d.used_by_desc_fun <- true ; 
		       var 
		     end
		  |_ -> var
	end

let verif_ref symbtbl name  =
  let rec aux1 name = match name with
    |Simple(s)-> 
       begin
	 find_var symbtbl s
       end
    |Multiple(s,b) -> aux1 s
  in 
  let var = aux1 name in
    begin
      match var with
	|Int(e)-> 
	   begin
	     match e.argtype_int with
		 By_Reference -> ()
	       |Local_Var -> ()
	       |_ -> failwith ("N est pas une référence")
	   end
	|Array(e) -> ()
	|Function(_) -> failwith ("pas un entier passer en référence")
    end
      

let verif_dim symbtbl name b =
  let rec aux1 name = match name with
    |Simple(s)-> 
	 find_var symbtbl s
    |Multiple(s,b) -> aux1 s
  in 
  let var = aux1 name in
    begin
      match var with
	|Int(e)-> 
	   begin
	     match name with
		 Simple(s)-> Inttype
	       |Multiple(_,_)-> failwith "Ne peut pas assigner un entier à un tableau"
	   end
	|Array(e) -> 
	   let rec aux2 typing acces = match typing,acces with
	     |Inttype,Simple(s) -> Inttype
	     |Arraytype(a1,(b1,c1)),Simple(s) -> if b then Arraytype(a1,(b1,c1)) else failwith "Votre tableau a une mauvaise dimension"
	     |Arraytype(a1,(b1,c1)),Multiple(a2,b2) -> aux2 a1 a2
	     |Inttype,Multiple(a2,b2) ->  failwith "Votres tableau a une mauvaise dimension"
	   in
	     aux2 e.typing name
	|Function(_) -> failwith "cette exception ne doit pas arriver"
    end

let rec correspond symbtbl a (b:arithm) = match a with 
  | Ref(v) -> 
      begin 
	match b with 
	  | Var(Simple vv) -> let var = find_var symbtbl vv in  (* la routine *)
	      begin
		match var with
		  | Int(d) -> d.used_by_descendance <- true
		  | _ -> failwith ("l'argument doit etre une variable representant un entier ("^v^")")
	      end
	  | _ -> failwith ("l'argument doit etre une variable ("^v^")")
      end
  | Val(v,Inttype) -> anal_stat_ari symbtbl b
  | Val(v,t) -> 
      begin 
	match b with (* t doit representer le type Array *) 
	  | Var(e) ->
	      let var = verif_dim symbtbl e true in 
	      let _ = verif_type symbtbl t var in ()
	  |_ -> failwith "un entier n'est pas un tableau"
      end	
and anal_stat_ari symbtbl (arithm:arithm) =
  match arithm with
    | Expr.Int(n) -> ()
    | Add(m,n) ->
	begin
	  anal_stat_ari symbtbl m;
	  anal_stat_ari symbtbl n
	end
    | Sub(m,n) ->
	begin
	  anal_stat_ari symbtbl m;
	  anal_stat_ari symbtbl n
	end
    | Times(m,n) ->
	begin
	  anal_stat_ari symbtbl m;
	  anal_stat_ari symbtbl n
	end
    | Div(m,n) ->
	begin
	  (*on aura la gestion du divide by zero plus tard*)
	  anal_stat_ari symbtbl m;
	  anal_stat_ari symbtbl n
	end
    | Var(name) -> let _ = verif_dim symbtbl name false in ()

    | App(name,list_argues) -> verif_function symbtbl name list_argues

and verif_function symbtbl name argues_list = 
  let var = find_var symbtbl name in
  let descr_var = match var with Function(d) -> d | _ -> failwith (" n'est pas une fonction") in
  let rec correspond_list l1 l2 = match l1, l2 with
    |[],[] -> ()
    |typ::typl, a::q -> correspond symbtbl typ a; correspond_list typl q
    |_ -> failwith ("Appel de avec un mauvais argument")
  in correspond_list descr_var.arg argues_list
	
(* verification des arithm : variables utilisées bien définies *)

let rec verif_ari_list symbtbl list = 
  match list with
    | [] -> ()
    | a::q -> 
	begin
	  anal_stat_ari symbtbl a;
	  verif_ari_list symbtbl q
	end

(*ambiguite de arithm pour supervector*)
(*la dimension : ca te va ?*)
let rec verif_init_arr ldim arr =
    match ldim, arr with
      |[], Int_sup(_)-> ()
      |(deb,fin)::q, Vector(t) -> let n= Array.length t in
	 if n<> fin-deb+1 then failwith "mauvaise initialisation de tableau"
	 else 
	   for i = 0 to n-1 do
	     verif_init_arr q (t.(i))
	   done
      |_->  failwith "mauvaise initialisation de tableau"

let rec anal_cond symbtbl cond =
  match cond with
    | Or(cond1, cond2) -> 
	begin
	  anal_cond symbtbl cond1;
	  anal_cond symbtbl cond2
	end
    | And(cond1,cond2) ->
	begin
	  anal_cond symbtbl cond1;
	  anal_cond symbtbl cond2
	end
    | Xor(cond1,cond2) ->
	begin
	  anal_cond symbtbl cond1;
	  anal_cond symbtbl cond2
	end
    | Not(cond1) ->
	begin
	  anal_cond symbtbl cond1;
	end
    | Op(op,ari1,ari2) ->
	begin
	  anal_stat_ari symbtbl ari1;
	  anal_stat_ari symbtbl ari2
	end

let rec analyse_statique symbtbl cmd =
  match cmd with
    |Seq([]) -> ()
    |Seq(c1::q) -> 
       begin
	 analyse_statique symbtbl c1;
	 analyse_statique symbtbl (Seq(q))
       end
    | Ifte(cond,c1,c2) -> 
	begin
	  anal_cond symbtbl cond;
	  analyse_statique symbtbl c1;
	  analyse_statique symbtbl c2
	end
    | While(cond,c1) ->
	begin
	  anal_cond symbtbl cond;
	  analyse_statique symbtbl c1
	end
    | For(name, ari1, ari2, c1) -> 
	begin
	  let _ = verif_dim symbtbl name false in
	  let _ = verif_ref symbtbl name in
	    analyse_statique symbtbl c1;
	    anal_stat_ari symbtbl ari1;
	    anal_stat_ari symbtbl ari2
	end
    | Print(printa) -> 
	begin
	  match printa with
	    |PInt(ari) -> anal_stat_ari symbtbl ari
	    |PString(s) -> ()
	end
    | Assign(name,ari) -> 
	begin
	  match name with
	    |Simple(s) -> 
	       begin
		 let var  = find_var symbtbl s in
		   match var with 
		       Int(descr_var) ->
			 begin
			   match descr_var.argtype_int with
			     | By_Value -> failwith ("modification d'un argument passé par valeur")
			     | _ -> ()
			 end;
			 anal_stat_ari symbtbl ari
		     | Function(d) -> anal_stat_ari symbtbl ari
		     | _ -> failwith (" ne peux etre assigne")
	       end
	    |Multiple(a,b) -> let _ = verif_dim symbtbl name false in anal_stat_ari symbtbl ari
	end
