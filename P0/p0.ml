
(*P0 TC: Xoel González Pereira. Grupo 3.2 *)

   (*EJERCICIO 1*)
(*-------------------*)

(* FUNCIÓN mapdoble *)

let rec mapdoble funcion1 funcion2 lista = match lista with 
        
         [] -> []
        
        | hd :: [] ->(funcion1 hd) :: []
        
        | hd1::hd2::tl ->(funcion1 hd1)::(funcion2 hd2)::(mapdoble funcion1 funcion2 tl)
        

        ;;
        
(* TIPO DE LA FUNCIÓN mapdoble *)

(* val mapdoble : ('a -> 'b) -> ('a -> 'b) -> 'a list -> 'b list = <fun>  *)

(*VALOR DE # mapdoble (function x -> x*2) (function x -> "x")# mapdoble (function x -> x*2) (function x -> "x") [1;2;3;4;5];;

Error: This expression has type string but an expression was expected of type
         int
*)

(* TIPO DE let y = function x -> 5 in mapdoble y;; 

# let y = function x -> 5 in mapdoble y;;
- : ('_weak1 -> int) -> '_weak1 list -> int list = <fun>  *)








   (*EJERCICIO 2*)
(*-------------------*)


(* FUNCIÓN primero_que_cumple *)

let rec primero_que_cumple predicado lista = 

  match lista with

   [] -> raise (Not_found)
   
   | hd::tl -> if ( predicado hd) then hd else primero_que_cumple predicado tl;;
 
   
(* TIPO DE LA FUNCIÓN primero_que_cumple 

 val primero_que_cumple : ('a -> bool) -> 'a list -> 'a = <fun> *)
   

(* FUNCIÓN existe *)

let existe predicado lista = 

  try primero_que_cumple predicado lista with

    Not_found ->false
   
     | _ ->true
   
   ;;
   
(* TIPO DE LA FUNCIÓN existe 
   
 val existe : (bool -> bool) -> bool list -> bool = <fun> *)


let asociado lista etiqueta = match lista with

    [] -> raise Not_found 

   |hd ::tl -> snd(primero_que_cumple(function (a,b)->if a = etiqueta then true else raise Not_found ) lista);;
   

(* TIPO DE LA FUNCIÓN asociado 

 val asociado : ('a * 'b) list -> 'a -> 'b = <fun> *)









   (*EJERCICIO 3*)
(*-------------------*)

(* ÁRBOL BINARIO *)
type  'a arbol_binario =

      Vacio
      
     |Nodo of 'a * 'a arbol_binario * 'a arbol_binario;;

 
 
(*FUNCIÓN in_orden *)

let rec in_orden = function
  | Vacio -> []
  | Nodo (value, left, right) -> in_orden left @ [value] @ in_orden right;;
 
(*val in_orden : 'a arbol_binario -> 'a list = <fun> *)



(*
let arbol= Nodo(3,Nodo(2,Vacio,Vacio),Nodo(5,Nodo(4,Vacio,Vacio),Nodo(1,Vacio,Vacio)));;
val arbol : int arbol_binario =
  Nodo (3, Nodo (2, Vacio, Vacio),
   Nodo (5, Nodo (4, Vacio, Vacio), Nodo (1, Vacio, Vacio)))
   
  *) 
  
  
 
 (*  
# in_orden arbol;;
- : int list = [2; 3; 4; 5; 1]

*)


(*FUNCIÓN pre_orden *)

let rec pre_orden = function
  | Vacio -> []
  | Nodo (value, left, right) -> [value] @ pre_orden left @ pre_orden right;;

(* val pre_orden : 'a arbol_binario -> 'a list = <fun> *)
 
 
(* 
#pre_orden arbol;;
- : int list = [3; 2; 5; 4; 1] *)




(*FUNCIÓN post_orden  *)  

let rec post_orden = function
  | Vacio -> []
  | Nodo (value, left, right) -> post_orden left @ post_orden right @ [value] ;;
  
 (* val post_orden : 'a arbol_binario -> 'a list = <fun> *)
  
(*  # post_orden arbol;;
- : int list = [2; 4; 1; 5; 3] *)


(*FUNCIÓN anchura*)

let anchura tree = 
	let rec loop visitados = function
		| [] -> List.rev visitados
		| Vacio::tl -> loop visitados tl
		| Nodo(x,h1,h2)::tl -> loop (x::visitados) (tl@[h1;h2])
	in loop [] [tree]
;;

(* val anchura : 'a arbol_binario -> 'a list = <fun> *)

(*
# anchura arbol;;
- : int list = [3; 2; 5; 4; 1]
*)





   (*EJERCICIO 4*)
(*-------------------*)


type 'a conjunto = Conjunto of 'a list;;


(*FUNCIÓN pertenece*)

let rec pertenece a = function

      Conjunto [] -> false
      
     | Conjunto (hd::tl) ->if (hd=a) then true  else pertenece a (Conjunto tl);;


(* val pertenece : 'a -> 'a conjunto -> bool = <fun> *)


(*FUNCIÓN agregar*)

let agregar a conjunto = 
	if (pertenece a conjunto) then
		conjunto
	else match conjunto with
		Conjunto nuevo -> Conjunto (a::nuevo)
;;

(*val agregar : 'a -> 'a conjunto -> 'a conjunto = <fun> *)


(*FUNCIÓN conjunto_of_list*)

let conjunto_of_list lista =
	let rec loop conjunto = function
		| [] -> conjunto
		| hd::tl -> loop (agregar hd conjunto) tl
	in loop (Conjunto []) lista
;;


(* val conjunto_of_list : 'a list -> 'a conjunto = <fun> *)





(*FUNCIÓN suprimir*)

let suprimir a (Conjunto c) =
	let rec loop nuevo conjunto =match conjunto with
		Conjunto (hd::tl) -> if a=hd then Conjunto (nuevo@tl)
				  else loop (hd::nuevo) (Conjunto tl)
		| Conjunto [] -> Conjunto c
	in loop [] (Conjunto c);;
	
(* val suprimir : 'a -> 'a conjunto -> 'a conjunto = <fun>  *)


(*FUNCIÓN cardinal*)

let cardinal (Conjunto c) =
	let rec loop suma conjunto = match conjunto with
		[] -> suma
		|hd::tl -> loop (suma+1) tl
		in loop 0 c
		
;;
 
(* val cardinal : 'a conjunto -> int = <fun> *)


(*FUNCIÓN union*)

let union (Conjunto c1) (Conjunto c2) =			
	let rec loop conjunto1 conjunto2 = match conjunto2 with
		[] -> (Conjunto conjunto1)
		| hd2::tl2 -> if (pertenece hd2 (Conjunto conjunto1)) then loop conjunto1 tl2
			     else loop (hd2::conjunto1) tl2
	in loop c1 c2;;


(* val union : 'a conjunto -> 'a conjunto -> 'a conjunto = <fun> *)

(*FUNCIÓN interseccion*)

let rec interseccion (Conjunto c1) (Conjunto c2) = match c1 with
	[] -> Conjunto []
	| hd::tl -> if pertenece hd (Conjunto c2) then
                         agregar hd (interseccion (Conjunto tl) (Conjunto c2))
                       else
                         interseccion (Conjunto tl) (Conjunto c2);;
                                                       
(* val interseccion : 'a conjunto -> 'a conjunto -> 'a conjunto = <fun> *)



(*FUNCIÓN diferencia*)

let diferencia (Conjunto c1) (Conjunto c2) =				
	let rec loop nuevo conjunto1 = match conjunto1 with
    [] -> Conjunto nuevo
    | hd1::tl1 -> if (pertenece hd1 (Conjunto c2)) then loop nuevo tl1
                else loop (hd1::nuevo) tl1
  in loop [] c1;;
		
		
(* val diferencia : 'a conjunto -> 'a conjunto -> 'a conjunto = <fun> *)



(*FUNCIÓN incluido*)

let rec incluido (Conjunto c1) (Conjunto c2) = match c1 with
	[] -> true
	| hd::tl -> if (pertenece hd (Conjunto c2)) then incluido (Conjunto tl) (Conjunto c2)
			  else false;;
			  	  
(* val incluido : 'a conjunto -> 'a conjunto -> bool = <fun> *)




(*FUNCIÓN igual*)

let igual (Conjunto c1) (Conjunto c2) = 
	if((diferencia (Conjunto c1) (Conjunto c2))= Conjunto []) then true else false;;

(* val igual : 'a conjunto -> 'a conjunto -> bool = <fun>  *)


(*FUNCIÓN producto_cartesiano*)

let producto_cartesiano (Conjunto c1) (Conjunto c2) = 
	let rec loop nuevo conjunto1 conjunto2 = match conjunto1,conjunto2 with
		[],_ -> Conjunto nuevo
		| hd1::_,hd2::tl2 -> loop ((hd1,hd2)::nuevo) conjunto1 tl2
		| _::tl1,[] -> loop nuevo tl1 c2
	in loop [] c1 c2;;
	
(* val producto_cartesiano : 'a conjunto -> 'b conjunto -> ('a * 'b) conjunto =
  <fun> *)
  

(*FUNCIÓN list_of_conjunto*)

let list_of_conjunto = function 
  
  (Conjunto c) -> c;;

(* val list_of_conjunto : 'a conjunto -> 'a list = <fun> *)
