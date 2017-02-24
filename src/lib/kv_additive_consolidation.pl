kv_additive_consolidation( KV, Consol ) :-
	kv_additive_consolidation( KV, Consol, [] ).

kv_additive_consolidation( [], Tail, Tail ).
kv_additive_consolidation( [K-V|T], [K-NwV|Follow], Tail ) :-
	kv_additive_consolidation_split( T, K, V, L, R, NwV ),
	kv_additive_consolidation( L, Follow, LTail ),
	kv_additive_consolidation( R, LTail, Tail ).

kv_additive_consolidation_split( [], _KPiv, NwV, [], [], NwV ).
kv_additive_consolidation_split( [Kh-Vh|T], KPiv, VAcc, L, R, NwV ) :-
	compare( Op, Kh, KPiv ),
	( Op == (=) ->
		NxVAcc is VAcc + Vh,
		TL = L,
		TR = R
		;
		NxVAcc is VAcc,
		( Op == (<) ->
			L = [Kh-Vh|TL],
			TR= R
			;
			TL= L,
			R = [Kh-Vh|TR] 
		)
	),
	kv_additive_consolidation_split( T, KPiv, NxVAcc, TL, TR, NwV ).
