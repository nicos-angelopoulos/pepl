kvis_val_left_incl( [], Mark, _Mval, _Left ) :-
	write( couldnt_find_key_at_end(Mark) ), abort.
kvis_val_left_incl( [Hk-Hv|T], Mark, Mval, Left ) :-
	( Hk < Mark -> 
		Left = [Hv|Teft], 
		kvis_val_left_incl( T, Mark, Mval, Teft )
		;
		( Hk =:= Mark -> 
			Mval = Hv, Left = [Hv]
			;
			write( couldnt_find_key(Mark) ), abort
		)
	).
