sample_refutation( Goal, Path, Prb ) :-
	sample( Goal, Path, Succ, Prb ),
	Succ \== fail.
	% also try, sample( Goal, Path, true, Prb )
	% refutational_path( Path ).

sample( Goal, FlatPath, Succ, Prb ) :-
	sample( Goal, 0, FlatPath, Succ, Prb ).
sample( Goal, Eps, FlatPath, Succ, Prb ) :-
	% functor( Goal, Name, Arity ),
	Goal =.. [Name|NmArgs],
	length( NmArgs, NmArity ),
	( Name/NmArity = phrase/3 ->
		NmArgs = [RealGoal|PhraseArgs],
		RealGoal =.. [RealName|RealArgs],
		append( RealArgs, PhraseArgs, Args ),
		length( Args, RealArity )
		;
		RealName = Name, RealArity is NmArity,
		Args = NmArgs
	),
	rc( sample, RealName/RealArity, ClId, Pr0 ),
	ExpGoal =.. [RealName,ClId,Pr0,Eps,sample,Path,Succ,Prb|Args],
	call( pepl_slp:ExpGoal ),
	flatten_nv( Path, FlatPath ).

most_likely_deduction( Goal, MaxDedu, MaxPrb ) :-
	Goal =.. [Name|Args],
	length( Args, Arity ),
	rc( max, Name/Arity, ClId, Pr0 ),
	ExpGoal =.. [Name,ClId,Pr0,max,MaxDedu,MaxPrb|Args],
	pepl_slp:ExpGoal.

most_probable_deduction( Goal, MaxDedu, MaxPrb ) :-
	descending_probability_deductions( Goal, MaxDedu, MaxPrb ),
	!.

descending_probability_deductions( Goal, Dedu, Prb ) :-
	Goal =.. [Name|Args],
	length( Args, Arity ),
	ExpGoal =.. [Name,ClId,Pr0,all,Path,Prb|Args],
	findall( Prb-(Path-Goal), 
		( 
		   rc( all, Name/Arity, ClId, Pr0 ),
		   pepl_slp:ExpGoal
		),
		RefPairs 

		   ),
	keysort( RefPairs, Ascends ),
	reverse( Ascends, Descents ),
     member( Prb-(Dedu-Goal), Descents ).

descending_probability_resolutions( Goal, Ref, Prb ) :-
	descending_probability_deductions( Goal, Ref, Prb ),
	refutational_path( Ref ). 

most_probable_resolution( Goal, MaxRef, MaxPrb ) :-
	descending_probability_resolutions( Goal, MaxRef, MaxPrb ),
	!.

refutational_path( Path ) :-
	flatten_nv( Path, FlatPath ),
	last( FlatPath, Last ),
	Last \== fail.
