:- ensure_loaded( '../prolog/pepl' ).
:- ensure_loaded( library('kv_additive_consolidation') ).

main :-
	dbg_pepl( (write(running(main_exact)), nl) ),
	main_exact.

main_exact :-
	fam( [ 	goal(bloodtype(_A)), slp( '../slp/prism_bt' ),
			data([bloodtype(a)-4,bloodtype(b)-2,
					bloodtype(o)-3,bloodtype(ab)-1
				]),
			count(exact), termin([iter(15)])
		] ).

main_store :-
	fam( [	goal(bloodtype(_A)), slp( '../slp/prism_bt' ),
			data([bloodtype(a)-4,bloodtype(b)-2,
					bloodtype(o)-3,bloodtype(ab)-1]),
			count(store), termin([iter(15)])
		  ] ).

main_sample :-
	fam( [	goal(bloodtype(_A)), slp( '../slp/prism_bt' ),
			data([bloodtype(a)-4,bloodtype(b)-2,
					bloodtype(o)-3,bloodtype(ab)-1]),
			count(sample), termin([iter(5)]),times(10000)
		   ] ).

test( Tms ) :-
	Goal = bloodtype(_A),
	experiment( [
		times(Tms),clause(scall(Goal,-inf,sample,Path,Succ,_Prb),Goal-Succ-Path),
		file('/dev/null'),filemode(append),rand(none,n,n),observables( Obs )
			], _Res ),
	list_frequency( Obs, Freqs ), dbg_ls( freqs, Freqs ),
	clean_fxp( Freqs, Clean ),
	kv_additive_consolidation( Clean, Conso ),
	dbg_ls( consolidated, Conso ).


clean_fxp( [], [] ).
clean_fxp( [At-Succ-_Path-Tms|T], Clean ) :-
	( Succ == fail -> 
		TClean = Clean
		;
		Clean = [At-Tms|TClean]
	),
	clean_fxp( T, TClean ).
