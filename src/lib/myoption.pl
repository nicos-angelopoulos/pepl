:- ensure_loaded( library(random) ).		%.

option( set_rand, InOpts, Def, OutOpts, Ret ) :-
	( select(rand(A,B,C),InOpts,OutOpts) -> 
		setrand( rand(A,B,C) )
		;
		( select(no_rand,InOpts,OutOpts) ->
			true
			;
			OutOpts = InOpts,
			setrand( Def )
		)
	),
	getrand(Ret),
	dbg( (write(getrand(Ret)),nl) ).

option_sel( Functor, InOpts, Def, OutOpts, Ret ) :-
	Term =.. [Functor,Ret],
	( select(Term,InOpts,OutOpts) ->
		true
		;
		Ret = Def, OutOpts = InOpts
	).

sel_option_actions( Term, Opts, Was, WasNot ) :-
	( memberchk(Term,Opts) ->
		call(Was)
		;
		call(WasNot)
	).
