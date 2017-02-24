:- requires( div_list/3 ).

normalise_list( List, Qt, NrmList ) :-
	( Qt =:= 0 -> 
		NrmList = List
		;
		div_list( List, Qt, NrmList )
	).
