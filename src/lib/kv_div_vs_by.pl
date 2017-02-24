kv_div_vs_by( [], _By, [] ).
kv_div_vs_by( [K-V|T], By, [K-NwV|Rst] ) :-
	NwV is V / By,
	kv_div_vs_by( T, By, Rst ).
