:- ensure_loaded( library(lists) ).		% append/3, reverse/2.

/* 
     basename( '/abc/def.jpg', 'def.jpg' ).

     */

basename( File, Base ) :-
	atom_codes( File, FileCs ),
	reverse( FileCs, ElifCs ),
	( append( EsabCs, [0'/|Tail], ElifCs ) ->
		( EsabCs==[] ->
			ToRev = Tail
			;
			ToRev = EsabCs
		),
		reverse( ToRev, BaseCs ),
		atom_codes( Base, BaseCs )
		;
		Base = File
	).
