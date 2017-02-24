% compare_list_selects( Input, Sel, Op, Rej, Selected, Rem ) :-
compare_list_selects( [], Tail, _Pred, [] , [], Tail ).
compare_list_selects( [Hs|Ts], [Hin|Tin], Pred, Rej, Selected, Rem ) :-
	Goal =.. [Pred,Hin,Hs],
	( call(Goal) ->
		NxS = Ts,
		TRej = Rej,
		Selected = [Hin|TSelected]
		;
		NxS = [Hs|Ts],
		Rej = [Hin|TRej],
		TSelected = Selected
	),
	compare_list_selects( NxS, Tin, Pred, TRej, TSelected, Rem ).

