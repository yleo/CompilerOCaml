open Expr
open Analyse
open Symbol_type
(* modifier les hashtbl.add *)    
let noun = ref 0  (*je donne des noms relatifs à out*)

let rec dim_of_type typing =
    match typing with
      |Inttype -> []
      |Arraytype(typing2, (deb, fin)) -> (deb,fin)::(dim_of_type typing2)

let rec iter l f = 
  match l with
    | [] -> ()
    | t::q -> begin f t; iter q f end

let rec feed current htbl defs location = match defs with
  | DefVal(name,typing,None)::q ->
      begin 
	match typing with
	  | Inttype ->
              begin
		Hashtbl.add htbl name (Int({argtype_int=Local_Var; num_arg_int = 0; location = location ; absolute_name_int = (!noun) ; used_by_descendance=false ; access_int=None}));
		incr noun
	      end  
	  | Arraytype(typing2, (i,j)) ->
	      begin
		let l = (i,j)::(dim_of_type typing2) in
		Hashtbl.add htbl name (Array( { argtype_array =Local_Var; location_array = location ; typing= Arraytype(typing2, (i,j)) ; num_arg_array = 0; dimension=l; absolute_name_array=(!noun) ; stack=None ; access_array = None}));
		  incr noun;
	      end
      end;
      feed current htbl q location

  | DefVal(name,typing,Some(ari_array))::q ->
      begin 
	match typing,ari_array with
	  | Inttype,Vector(_) -> failwith "initialisation d'un tableau pour un entier"
	  | Arraytype(_),Int_sup(_) -> failwith "initialisation d'un entier pour un tableau"
	  | Inttype, Int_sup(ari) ->
              begin
		anal_stat_ari current ari;
		Hashtbl.add htbl name (Int({argtype_int=Local_Var; num_arg_int = 0; location=location ; absolute_name_int = (!noun) ; used_by_descendance=false ; access_int=None}));
		incr noun;
	      end 
	  | Arraytype(typing2, (i,j)), vect ->
	      begin
		let ldim = (i,j)::(dim_of_type typing2) in
		  verif_init_arr ldim vect; (* regarde si dans current, arr est de type typing2 et retourne la dimension du tableau *)
		  Hashtbl.add htbl name (Array( { argtype_array =Local_Var; typing= Arraytype(typing2, (i,j)); location_array=location  ; num_arg_array = 0; dimension=ldim; absolute_name_array=(!noun) ; stack=None ; access_array = None }));
		  incr noun
	      end
      end;
      feed current htbl q location
  | DefFunc(name_func,argl,defl,cmd)::q ->
      begin
	let htbl2 = Hashtbl.create(10) in
	let next = Locale(current,htbl2) in
        let numero = ref 0 in
	  iter argl (fun arg ->  match arg with
	    | Val(name,Inttype) -> 
		begin
		  
		  Hashtbl.add htbl2 name (Int({argtype_int=By_Value; num_arg_int =(!numero) ;location=name_func ; absolute_name_int =(!noun) ; used_by_descendance=false ; access_int=None}));
		  incr numero;
		  incr noun;
		end;
	    | Val(name,Arraytype(typing2,(i,j))) ->
 		begin
		  Hashtbl.add htbl2 name (Array({ argtype_array =By_Value; typing=Arraytype(typing2,(i,j)) ;location_array=name_func  ; num_arg_array = (!numero); dimension=[] ;absolute_name_array =(!noun) ; stack=None ; access_array = None }));
		  incr numero;
		  incr noun;
		end;
	    | Ref(name) -> 
		begin 
		  Hashtbl.add htbl2 name  (Int({argtype_int=By_Reference; num_arg_int =(!numero) ;location=name_func ;absolute_name_int =(!noun) ; used_by_descendance=false ; access_int=None}));
		  incr numero;
		  incr noun;
		end
		    );
	  feed next htbl2 defl name_func;
	  Hashtbl.add htbl name_func  (Function({arg=argl ; number_arg = List.length argl; parent=next ; absolute_name_fun=(!noun) ; offset=16 ; body=None ; access_func=None ; used_by_desc_fun=false ; reg_used=[] ; printed = false ; correspondance = None}));
	  incr noun;
	  analyse_statique next cmd;
	  feed current htbl q name_func
      end
  | [] -> ()

let rec create prgm =
  let (htbl:((string,descriptor) Hashtbl.t)) =
    Hashtbl.create (10) in
  let defs,cmd = prgm in
  let to_return = Root(htbl) in
    Hashtbl.add htbl "program" (Function({arg=[] ; number_arg = 0 ; parent=to_return ; absolute_name_fun = -1 ; offset=16 ; body=None ; access_func=None ; used_by_desc_fun=false; reg_used=[] ; printed = false; correspondance = None}));
    feed to_return htbl defs "program";
    analyse_statique to_return cmd;
    to_return

(* Ajouter program comme fonction dans root... *)
