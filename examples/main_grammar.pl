:- ensure_loaded( '../prolog/pepl' ).

main :-
	main_exact.

main_exact :-
	fam( [
				goal(phrase(s,_A,[])),
				slp( '../slp/grammar' ),
				datafile('data/grammar_data'),
				termin([iter(3)])
		  ] ).
