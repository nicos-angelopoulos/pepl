:- ensure_loaded( '../prolog/pepl' ).

/* Sept 2006.
     Yap and Swi agree in giving different results for 
     main_exact and main_sample.
*/

main :-
	dbg_pepl( (write(running(main_exact)), nl) ),
	main_exact.

main_exact :-
	fam( [goal(s(_A)),
		 data([s(a)-7,s(b)-5]),
		 count(exact),termin([iter(5)]),slp('../slp/jc_ml_S2')
		] ).

main_store :-
	fam( [ goal(s(_A)),
		  data([s(a)-7,s(b)-5]),
		  count(store),termin([iter(5)]),slp( '../slp/jc_ml_S2' )
		] ).

main_sample :-
	fam( [ goal(s(_A)),
		  data([s(a)-7,s(b)-5]),
		  count(sample), termin([iter(5)]),
		  times(1000),slp( '../slp/jc_ml_S2' )
		  ] ).
