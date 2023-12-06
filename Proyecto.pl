 /*-----------------------------------------------------------------------------
                                 Operadores
-----------------------------------------------------------------------------*/

:- op(10,xfx,':').
:- op(15,xfx,'=>').
:- op(20,xfx,'=>>').

/*----------------------------------
Abrir y guardar base de conocimiento
----------------------------------*/

open_kb(Route,KB):-
	open(Route,read,Stream),
	readclauses(Stream,X),
	close(Stream),
	atom_to_term(X,KB).
save_kb(Route,KB):-
	open(Route,write,Stream),
	writeq(Stream,KB),
	close(Stream).

readclauses(InStream,W) :-
        get0(InStream,Char),
        checkCharAndReadRest(Char,Chars,InStream),
	atom_chars(W,Chars).

checkCharAndReadRest(-1,[],_) :- !.  % End of Stream
checkCharAndReadRest(end_of_file,[],_) :- !.

checkCharAndReadRest(Char,[Char|Chars],InStream) :-
        get0(InStream,NextChar),
        checkCharAndReadRest(NextChar,Chars,InStream).

%compile an atom string of characters as a prolog term
atom_to_term(ATOM, TERM) :-
	atom(ATOM),
	atom_to_chars(ATOM,STR),
	atom_to_chars('.',PTO),
	append(STR,PTO,STR_PTO),
	read_from_chars(STR_PTO,TERM).
/*-----------------------------------------------------------------------------
                             Manejo de atributos
-----------------------------------------------------------------------------*/
%atributo(V,Kb,X) - Determina el valor de un atributo
%V: Propiedad a buscar
%Kb: Lista con el identificador, propiedades y relaciones de un objeto
%X: Valor de la propiedad
atributo(V,[H|T],X):-
    H = V=>X;
    atributo(V,T,X).

%atributos(V,Kb,X) - Determina todos los valores de una lista de atributos
%                    (Por ejemplo, encontrar todos los id de la lista)
%V: Propiedad a buscar
%Kb: Lista de todos los objetos de una clase
%X: Valor de la propiedad
atributos(_,[],[]):-!.
atributos(V,[H|T],[X|TS]):-
    atributo(V,H,U),
    X = U,
    atributos(V,T,TS).

/*-----------------------------------------------------------------------------
                              Manejo de clases
-----------------------------------------------------------------------------*/
%madre(Clase,Kb,Madre) - Determina la madre de una clase
%Clase: Clase de interés
%Kb: Base de conocimiento
%Madre: Madre de la clase de interés
madre(_,[],_).
madre(Y,[class(Y,X,_,_,_)|_],X):-!.
madre(Clase,[_|T],Madre):-
    madre(Clase,T,Madre).

%hijos(Madre,Kb,Hijo) - Determina los hijos de una clase
%Madre: Clase de interés
%Kb: Base de conocimiento
%Hijo: Lista con los hijos de la clase de interés
hijos(_,[],[]).
hijos(Y,[class(X,Y,_,_,_)|T],[X|TS]):-
    hijos(Y,T,TS),!.
hijos(Madre,[_|T],Hijo):-
    hijos(Madre,T,Hijo).

%ext_clase(Clase,Kb,Objetos) - Determina los objetos de una clase
%Clase: Clase de interés
%Kb: Base de conocimiento
%Objetos: Variable de salida
ext_clase(_,[],[]):-fail.
ext_clase(Y,[class(Y,_,_,_,[H|T])|_],X):-
    atributos(id,[H|T],X),!.
ext_clase(Clase,[_|T],Objetos):-
    ext_clase(Clase,T,Objetos).

/*----------------------------------------------------------------------------------
Solución del inciso a) de la pregunta 1:

extension_clase(Clase,Kb,Objetos) - Determina todos los objetos de una clase,
                                    incluyendo la herencia
Clase: Clase de interés
Kb: Base de conocimiento (archivo entre comillas)
Objetos: Objetos de la clase de interés, incluyendo la herencia
----------------------------------------------------------------------------------*/
e_c(dummy,[],[_|_],[]).
e_c(dummy,[Hij|Os],[H|T],Objetos):-
    e_c(dummy,Os,[H|T],Objeto),
    e_c(Hij,[H|T],O),
    append(O,Objeto,Objetos),!.
e_c(_,[],[]).
e_c(Clase,[H|T],Objetos):-
    ext_clase(Clase,[H|T],Objetos);
    hijos(Clase,[H|T],Hijos),
    e_c(dummy,Hijos,[H|T],Objetos).
extension_clase(Clase,Kb,Objetos):-
    open_kb(Kb,X),
    save_kb(Kb,KB),
    open_kb(Kb,KB),
    e_c(Clase,KB,Objetos),
    save_kb(Kb,Y).

