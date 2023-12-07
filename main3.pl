%--------------------------------------------------
% Load and Save from files
%--------------------------------------------------


%KB open and save

open_kb(Route,KB):-
	open(Route,read,Stream),
	readclauses(Stream,X),
	close(Stream),
	atom_to_term(X,KB).

save_kb(Route,KB):-
	open(Route,write,Stream),
	write(Stream, Term),
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


/* Creación de predicados para eliminar 

a) Clases objectos
b) Propiedades específicas
c) Relaciones específicas
*/

/*
a) Eliminación Clases u objetos cuyo nombre 
será rm_class y rm_object, respectivamente. 
Ambos predicados recibirán tres argumentos: 
(i) el nombre de la clase u objeto a eliminar, 
(ii) la base de conocimientos actual,
(iii) la nueva base de conocimientos donde 
se refleja la eliminación.
*/


/*rm_class(Class,KB,NewKB):-
    delete(class(Class,UClass,_,_,_),KB).

rm_object_property(Class,KB,NewKB):-

rm_object_relation(Class,KB,NewKB):-*/

/*Rerecusividad para eliminar */

delete(_,[],[]).
delete(X,[X|Y],Z):-
    delete(X,Y,Z).
delete(A,[B|C],[B|D]):-
    delete(A,C,D).