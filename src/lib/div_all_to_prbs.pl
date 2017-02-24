div_all_to_prbs( [], _Div, [] ).
div_all_to_prbs( [H|T], Div, [Hm|Tm] ) :-
	Hm is H / Div,
	( Hm >= 0, Hm =< 1 ->
		true
		;
		throw( normalised_to_not_a_probability(Hm,H,Div,T) )
	),
	div_all_to_prbs( T, Div, Tm ).
