kvsi_val_w_right_rem( [], _Mark, _Mv, _Right ) :-
	write( error_of_emptyness ), abort.
kvsi_val_w_right_rem( [Hk-Hv|T], Mark, Mv, Right ) :-
	( Hk < Mark -> 
		kvsi_val_w_right_rem( T, Mark, Mv, Right )
		;
		( Hk =:= Mark -> 
			Mv = Hv,
			Right = T
			;
			write( error_of_pairs ), abort
		)
	).
