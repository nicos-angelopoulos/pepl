:- ensure_loaded( '../prolog/pepl' ).

main :-
	dbg_pepl( (write(running(main_exact)), nl) ),
	main_exact.

main_exact :-
	fam( [goal(s(_A,_B)),
		 data([s(a,p)-4,s(b,p)-2,s(a,q)-3,s(b,q)-3]),
		 % data([s(a,p)-4,s(b,p)-2,s(a,q)-3,s(b,q)-3]),
		 count(exact),termin([iter(5)]),slp(jc_ml_S1),
		 return([initial_pps(Init),final_pps(Fin)])
		] ),
	write( initial(Init) ), nl,
	write( final(Fin) ), nl.

main_store :-
	fam( [ goal(s(_A,_B)),
		  data([s(a,p)-4,s(b,p)-2,s(a,q)-3,s(b,q)-3]),
		  count(store),termin([iter(5)]),slp( '../slp/jc_ml_S1' )
		] ).

main_sample :-
	fam( [ goal(s(_A,_B)),
		  data([s(a,p)-4,s(b,p)-2,s(a,q)-3,s(b,q)-3]),
		  count(sample), termin([iter(5)]),
		  times(1000),slp( '../slp/jc_ml_S1' )
		  ] ).
