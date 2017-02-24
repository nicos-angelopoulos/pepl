:- ensure_loaded( library(lists) ).	% select/3.

kv_additive_consolidation( [], [] ).
kv_additive_consolidation( [K-V|T], Consol ) :-
	( select(K-V1,T,ResT) ->
		V2 is V1 + V,
		kv_additive_consolidation( [K-V2|ResT], Consol )
		;
		Consol = [K-V|Tconsol],
		kv_additive_consolidation( T, Tconsol )
	).
