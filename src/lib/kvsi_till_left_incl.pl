kvsi_till_left_incl( [Hk-Hv|T], MarK, Left ) :-
	( MarK =:= Hk -> 
		Left = [Hk-Hv]
		;
		Left = [Hk-Hv|Teft],
		kvsi_till_left_incl( T, MarK, Teft )
	).
