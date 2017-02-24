kv_replace_first_vals_w_rem( [], Tps, [], Tps ).
kv_replace_first_vals_w_rem( [H|T], [Hk-_Hv|Tps], [Hk-H|Tnwps], Rest ) :-
	kv_replace_first_vals_w_rem( T, Tps, Tnwps, Rest ).
