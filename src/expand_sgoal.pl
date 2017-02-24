expand_sgoal( Head, ExpHead ) :-
	expand_sgoal( Head, _, _, _, _, _, _, ExpHead ).

expand_sgoal( Head, Spec, ClId, Pr0, Eps, Sel, Path, Succ, Prb, ExpHead ) :-
	Head =.. [Name|NmArgs],
	length( NmArgs, NmArity ),
	( Name/NmArity = phrase/3 ->
		NmArgs = [RealHead|PhraseArgs],
		RealHead =.. [RealName|RealArgs],
		append( RealArgs, PhraseArgs, Args ),
		length( Args, RealArity )
		;
		RealName = Name, RealArity is NmArity,
		Args = NmArgs
	),
	Spec = RealName/RealArity,
	ExpHead =.. [RealName,ClId,Pr0,Eps,Sel,Path,Succ,Prb|Args].
