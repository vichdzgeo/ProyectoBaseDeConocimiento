%atributos(ObjetoObjetivo,Kb,ObjetosResultantes) 
objetos(_,[],[]):-!.
objetos(V,[H|T],[X|TS]):-
    objetos(V,H,U),
    X = U,
    objetos(V,T,TS).
%ext_clase(Clase,Kb,Objetos)
%Caso Base: La kb está vacía o terminó de recorrerse.
ext_clase(_,[],[]):-fail.
%Caso Recursivo: L clase coincide con su nombre, 
ext_clase(Clase,[class(Clase,_,_,_,Objetos)|_],ResultadoObjetos):-
    objetos(id,Objetos,ResultadoObjetos),!.
ext_clase(Clase,[_|SiguienteClase],ResultadoObjetos):-
    ext_clase(Clase,RestoObjetos,ResultadoObjetos).