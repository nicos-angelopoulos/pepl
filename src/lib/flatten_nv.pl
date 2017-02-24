% Different than Swi's flatten as that variables are take out.
%%%%%%%%
% flatten_nv( +NestedLists, ?FlatList ) :-
% FlatList is the list of all not list elements of 
% the lists in NestedLists, in depth first, left to right
% order, from o'keefe's book p. 97-98
%%%%%%%
%
flatten_nv( Nlist, Flist ) :-
	flatten_nv( Nlist, Flist, [] ).
flatten_nv( [], L0, L ) :-
	!,
	L0 = L.
flatten_nv( [H|T], L0, L ) :-
	!,
	flatten_nv( H, L0, L1 ),
	flatten_nv( T, L1, L ).
flatten_nv( Oth, [Oth|List], List ).
