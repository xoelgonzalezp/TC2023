
(* Práctica 1 TC : Xoel González Pereira  Grupo 3.2 *)

##load "talf.cma";;
# open Conj;;
# open Auto;;
# open Ergo;;
# open Graf;;


(*

let automata2 = af_of_string "0 1 2; a b c;0 ;2; 0 0 a; 0 1 b; 0 2 epsilon; 1 1 a; 1 0 b; 1 2 c; 2 2 a; 2 1 b; 2 0 c;";; (*tiene epsilon transicion*)
let automata2 = af_of_string "0 1 2 3; a b; 0; 3; 0 1 a; 0 2 b; 1 1 a; 1 0 b; 2 3 a; 2 2 b; 3 0 a; 3 3 b;";;
(*no tiene epsilon transicion*)


let automata3 = af_of_string "0 1 2; a b; 0; 2;0 0 a; 0 1 a; 0 1 b; 1 1 a; 1 2 b; 2 2 a; 2 0 b; ";;(*tiene no determinismo*)
let automata3 = af_of_string "0 1 2; a b; 0; 2;0 2 epsilon; 0 1 a; 0 1 b; 1 1 a; 1 2 b; 2 2 a; 2 0 b; ";;
(*no tiene no determinismos*)




let automata1 = af_of_string "0 1 2 3; a b; 0; 3; 0 1 a; 0 2 b; 1 1 a; 1 0 b; 2 3 a; 2 2 b; 3 0 a; 3 3 b;";; (*es afd*)
let automata1 = af_of_string "0 1 2 3; a b; 0; 3; 0 1 a;0 0 a; 0 2 b; 1 1 a; 1 0 b; 2 3 a; 2 2 b; 3 0 a; 3 3 b;";;
(*no es afd*)


(*ejemplo de autómatas equivalentes*)
let automata4 = af_of_string "0 1; a b; 0; 1; 0 1 a; 1 0 b; 1 1 a;";;
let automata5 = af_of_string "0 1 2; a b; 0; 1; 0 1 a; 1 2 b; 2 1 a; 2 0 b;";;

(*ejemplo de autómatas no equivalentes*)
let automata4 = af_of_string "0 1; a b; 0; 1; 0 1 a; 1 0 b; 1 1 a;";;
let automata5 = af_of_string "0 1 2; a b; 0; 2; 0 1 a; 1 2 b; 2 1 a; 2 0 b;";;



(*ejemplo escaner_afd*)
let cadena9 = cadena_of_string "s b s";;
let automata9 = af_of_string "0 1 2; s b; 0; 2; 0 1 s; 1 2 b; 2 2 s;";;
escaner_afd cadena9 automata9;;

(*ejemplo de no escaner_afd*)
let cadena9 = cadena_of_string "s b z";;
let automata9 = af_of_string "0 1 2; s b ; 0; 2; 0 1 s; 1 2 b; 2 2 s;";;
escaner_afd cadena9 automata9;;




(*ejemplo escaner_afn*)
let cadena10 = cadena_of_string "a b a";;
let automata3 = af_of_string "0 1 2; a b; 0; 2;0 0 a; 0 1 a; 0 1 b; 1 1 a; 1 2 b; 2 2 a; 2 0 b; ";;
escaner_afn cadena10 automata3;;

(*ejemplo de no escaner_afn*)
let cadena10 = cadena_of_string "a b ";;
let automata3 = af_of_string "0 1 2; a b; 0; 2;0 0 a; 0 1 a; 0 1 b; 1 1 a; 1 2 b; 2 2 a; 2 0 b; ";;
escaner_afn cadena10 automata3;;




let automata4 = Af (
    Conjunto [Estado "0"; Estado "1"; Estado "2"],
    Conjunto [Terminal "x"; Terminal "y"],
    Estado "0",
    Conjunto [        Arco_af (Estado "0", Estado "1", Terminal "x");        Arco_af (Estado "0", Estado "0", Terminal "y");        Arco_af (Estado "1", Estado "2", Terminal "x");        Arco_af (Estado "1", Estado "1", Terminal "y");        Arco_af (Estado "2", Estado "0", Terminal "x");        Arco_af (Estado "2", Estado "2", Terminal "y")    ],
    Conjunto [Estado "0"; Estado "1"; Estado "2"]
);;



*)


  
(*Ejercicio 1*
 ---------------*)
 
(*FUNCIÓN es_afne*)

let es_afne (Af (_,_,_,arcs,_)) =
	let rec loop = function
		Conjunto ((Arco_af(_,_,terminal))::tl) ->if terminal = (Terminal "") then true
			else
				loop (Conjunto tl)
		| Conjunto _ -> false
	in loop arcs
;;

(*val es_afne : Auto.af -> bool = <fun>*)


(*FUNCIÓN es_afn*)

let es_afn (Af (_,_,_,arcs,_)) =
	let rec loop conj boolean = function
		Conjunto ((Arco_af(s1,_,terminal))::tl) ->
			if terminal = (Terminal "") then
				loop conj boolean (Conjunto(tl))
			else if pertenece (s1,terminal) conj then
				loop (agregar (s1,terminal) conj) true (Conjunto(tl))
			else
				loop (agregar (s1,terminal) conj) boolean (Conjunto(tl))
		| Conjunto _ -> 
		boolean
	in loop (Conjunto []) false arcs
;;

(*val es_afn : Auto.af -> bool = <fun>*)


let tiene_arco (estado,simbolo) transiciones =
	let rec recorrer count = function
		| Conjunto [] -> count = 1
		| Conjunto (Arco_af (e,_, s)::tl) when estado=e && simbolo=s && s<>(Terminal "") && s<>(Terminal "epsilon")->
				(count > 1) || recorrer (count+1) (Conjunto tl)
		| Conjunto (Arco_af _::tl) -> 
				recorrer count (Conjunto tl)
	in recorrer 0 transiciones;;
	
(*val tiene_arco : Auto.estado * Auto.simbolo -> Auto.arco_af Conj.conjunto -> bool = <fun>*)	



let es_afd (Af (estados,simbolos,_,transiciones,_)) =
	let rec recorrer_estados = function
		| Conjunto [] -> true
		| Conjunto ((Estado est)::estado_tl) ->
			let rec recorrer_simbolos = function
				| Conjunto [] -> true
				| Conjunto ((Terminal s)::simbolo_tl) -> 
						if (tiene_arco (Estado est, Terminal s) transiciones) then
							recorrer_simbolos (Conjunto simbolo_tl)
						else
							false
				| Conjunto ((No_terminal s)::_) -> 
						false
			in recorrer_simbolos simbolos && recorrer_estados (Conjunto estado_tl)
	in recorrer_estados estados;;

(*val es_afd : Auto.af -> bool = <fun>*)


(* Ejercicio 2
----------------*)


let rec transicion estado value (Conjunto arcoAf) = match arcoAf with 
      [] -> estado
    | Arco_af(estado1,estado2,entrada)::t -> if estado1 = estado && entrada = value then 
                                if (entrada <> Terminal "") && (estado1 = estado2) then 
                                  transicion estado value (Conjunto t)
                                else 
                                  estado2
                              else transicion estado value (Conjunto t)
;;

(*val transicion : Auto.estado -> Auto.simbolo -> Auto.arco_af Conj.conjunto -> Auto.estado = <fun>*)



let equivalentes (Af(_,simbolos1, inicial1, arcos1, finales1)) (Af(_, simbolos2, inicial2,arcos2, finales2)) =
  let alfabeto = agregar (Terminal "") (union simbolos1 simbolos2) in 
  let rec loop (Conjunto cola) visitados =  match cola with
      [] -> true
    | (estadoactual1, estadoactual2)::t ->  if pertenece (estadoactual1, estadoactual2) visitados then 
                  loop (Conjunto t) visitados
                else if (pertenece estadoactual1 finales1 && not (pertenece estadoactual2 finales2)) 
                || (not (pertenece estadoactual1 finales1) && pertenece estadoactual2 finales2) then 
                  false
                else
                  let vistos1 = agregar (estadoactual1, estadoactual2) visitados in 
                  let rec auxiliar (Conjunto alf) vistos= match alf with
                        [] -> true
                      | a::l -> let nuevoestado1 = transicion estadoactual1 a arcos1 in 
                                let nuevoestado2 =  transicion estadoactual2 a arcos2 in
                                if (not (pertenece (nuevoestado1, nuevoestado2) vistos)) then
                                  if (a<>(Terminal "") && (nuevoestado1 = estadoactual1 || nuevoestado2 = estadoactual2)) then 
                                    auxiliar (Conjunto l)  vistos 
                                  else 
                                    loop (agregar (nuevoestado1, nuevoestado2) (Conjunto cola)) vistos 
                                else auxiliar (Conjunto l)  vistos 
                  in auxiliar alfabeto vistos1
  in loop (Conjunto [(inicial1,inicial2)])  (Conjunto []) 
;;

(*val equivalentes : Auto.af -> Auto.af -> bool = <fun>*)





(*Ejercicio opcional*
-----------------------*)

(*FUNCIÓN escaner_afn*)

let escaner_afn cadena (Auto.Af (_, _, inicial, _, finales) as a) =

   let rec loop = function

        (Conj.Conjunto [], _) ->
           false

      | (actuales, []) ->
           not (Conj.es_vacio (Conj.interseccion actuales finales))

      | (actuales, simbolo :: t) ->
           loop (Auto.avanza simbolo actuales a, t)

   in
      loop (Conj.Conjunto [inicial], cadena)
   ;;


let sig_estado estado simbolo automata =
    let arcs = match automata with
      | Af (_,_,_,t,_) -> t
    in
    let rec loop = function
      | Conjunto [] -> raise Not_found
      | Conjunto (h::t) ->
        let i = match h with
          | Arco_af (inn,_,_) -> inn
        in
        let s = match h with
          | Arco_af (_,_,s) -> s
        in
        if i = estado && s = simbolo then
          match h with
            | Arco_af (_,enn,_) -> enn
        else
          loop (Conjunto t)
    in
    loop arcs
  ;;  

(*val sig_estado : Auto.estado -> Auto.simbolo -> Auto.af -> Auto.estado =<fun>*)


(*FUNCIÓN escaner_afd*)

  let escaner_afd cadena (Af (_, _, inicial, _, finales) as a) =
    let rec loop estado = function
      | [] -> not (es_vacio (interseccion (Conjunto [estado]) finales))
      | h::t -> 
        try 
         let next = sig_estado estado h a in 
           loop next t
        with Failure s  -> false
    in loop inicial cadena
  ;;
  
(*val escaner_afd : Auto.simbolo list -> Auto.af -> bool = <fun>*)





  
