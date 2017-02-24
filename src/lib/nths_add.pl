% nths_add( Nths, List, Elems, New ) :-
%
% ==
% nths_add( [1,2], [a,b,c,d,f], [x,y], New )
% New = [x,a,y,b,c,d,f] ? ;
% ==
%
nths_add( Nths, List, Elems, New ) :-
     nths_add( Nths, 1, List, Elems, New ).

nths_add( [], _I, List, [], List ) :- !.
nths_add( [Nth|TNths], I, List, Elems, NewList ) :-
     Nth =:= I,
	!,
     Elems = [He|TElems],
     NewList = [He|RemNew],
     nths_add( TNths, I, List, TElems, RemNew ).
nths_add( Nths, I, [H|T], Elems, [H|RemNew] ) :-
     NxI is I + 1,
	nths_add( Nths, NxI, T, Elems, RemNew ).
