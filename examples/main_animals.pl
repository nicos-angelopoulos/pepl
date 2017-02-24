:- ensure_loaded( '../prolog/pepl' ).

main :-
	main_exact. 

main_exact :-
	fam( [datafile('data/animals_data'),
		 slp('../slp/animals'), termin([iter(3)])] ).

main_store :-
	fam( [datafile('data/animals_data'),
		 slp('../slp/animals'), termin([iter(3)]), count(store) ] ).
