kv_sum_vs( [_K-V|T], Sum ) :-
	kv_sum_vs_1( T, V, Sum ).

kv_sum_vs_1( [], Sum, Sum ).
kv_sum_vs_1( [_-V|T], Acc, Sum ) :-
	NxAcc is Acc + V,
	kv_sum_vs_1( T, NxAcc, Sum ).
