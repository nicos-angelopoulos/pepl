kv_multi_vs_by( [], _By, [] ).
kv_multi_vs_by( [K-V|T], By, [K-M|R] ) :-
	M is V * By,
	kv_multi_vs_by( T, By, R ).
