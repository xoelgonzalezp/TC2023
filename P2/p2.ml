
(* Práctica 2 TC : Xoel González Pereira  Grupo 3.2 *)

##load "talf.cma";;
# open Conj;;
# open Auto;;
# open Ergo;;
# open Graf;;
  
  
(*EJERCICIO 1*
 ---------------*) 
  
let gramatica = gic_of_string "S A B C D; a b c d; S; S ->  A B | C D ; A -> a ; B -> b ;C -> c; D -> d;";;
(*es_fnc, da true*)
let gramatica = gic_of_string "S A C ; a c ;S; S -> A C ; A -> a ; C -> c;";; (*es fnc,da true*)
let gramatica = gic_of_string "S A C ; a c ;S; S -> A C ; A -> a ; C -> c | epsilon;";; (*no fnc, da false*)
let gramatica = gic_of_string "S A;a;S;S -> A; A -> a;";;(*no fnc, da false*) 

let es_fnc (Gic (noterminal, terminal, regla, inicial)) =

  let rec loop = function
    Conjunto [] -> true
  | Conjunto (Regla_gic (n, [No_terminal noterminal1; No_terminal noterminal2])::tl) ->
  
  
    if not (pertenece (No_terminal noterminal1) noterminal) then false
    else if not (pertenece (No_terminal noterminal2) noterminal) then false
    else loop (Conjunto tl)
    
  | Conjunto (Regla_gic (n, [Terminal terminal1])::tl) ->
    if not (pertenece (Terminal terminal1) terminal) then false
    else loop (Conjunto tl)
    
  | _ -> false
  
  in loop regla;;

(*val es_fnc : Auto.gic -> bool = <fun>*)


(*EJERCICIO 2*
 ---------------*)


let gramatica = gic_of_string "S A B C; x y; S; S -> A B | B C; A -> B A | x; B -> C C | y; C -> A B | x;";;
(*es fnc*)
let gramatica = gic_of_string " S A B C; a b; S; S -> A B | B C;  A -> B A | a; B -> C C | b; C -> A B | a;";;
let cadena3 = cadena_of_string "b b a b";;
let cadena1 = cadena_of_string "y x x y x";; (* da true *)
let cadena2 = cadena_of_string "y x x y z";; (* da false *)
let gramatica = gic_of_string "S A;a;S;S -> A; A -> a;";;(*no fnc, da excepción*) 


let cyk cadena gramatica =

  let ejec =

    let n = List.length cadena in

    let tabla = Array.make_matrix n n (Conjunto []) in

    let (Gic (_,_,prod,_)) = gramatica in 

    let reglas = list_of_conjunto prod in 

    for columna = 0 to n-1 do

      let simbolo = List.nth cadena columna in

      let lista_noterminales simbolo = match simbolo with

        | Terminal terminal ->

            let no_term_primera_regla = 

              List.filter (function Regla_gic(_, [Terminal t]) -> terminal = t | _ -> false) reglas in

            List.map (fun (Regla_gic(noterminal, _)) -> noterminal) no_term_primera_regla 

        | _ -> raise (Invalid_argument "Simbolo no válido")

      in
      tabla.(0).(columna) <- Conjunto (lista_noterminales simbolo)

    done;

    for fila = 1 to n-1 do

      for columna = 0 to n-1-fila do

        for k = 0 to fila-1 do

          let noterminal1 = tabla.(k).(columna) in

          let noterminal2 = tabla.(fila-k-1).(columna+k+1) in

          let noterminales = list_of_conjunto (cartesiano noterminal1 noterminal2) in

          let resultado = 

            List.map ( fun (noterm1,noterm2) -> 

              let lista_noterminales = function

                | (nonterminal1,nonterminal2) ->

                    let r = List.filter (function Regla_gic(_, l_noterminales) -> l_noterminales = [nonterminal1;nonterminal2]) reglas in

                    List.map (fun (Regla_gic(noterminal, _)) -> noterminal) r
              in

              lista_noterminales (noterm1,noterm2) |> List.sort_uniq compare

            ) noterminales

          in tabla.(fila).(columna) <- union (tabla.(fila).(columna)) (Conjunto (List.concat resultado))
        done;
      done;
    done;

    pertenece (No_terminal "S") tabla.(n-1).(0) 

  in

  if es_fnc gramatica  then

    try ejec with

        Invalid_argument _ -> false
  else
    raise (Invalid_argument "No válido");;

(*val cyk : Auto.simbolo list -> Auto.gic -> bool = <fun>*)
                                                                                     
