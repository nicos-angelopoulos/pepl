:- ensure_loaded( library(kvsi_replace_v) ).		% /3.

kvsi_replace_vs( [], Kvsi, Kvsi ).
kvsi_replace_vs( [K-V|T], InKvsi, OutKvsi ) :-
	kvsi_replace_v( InKvsi, K, V, NxKvsi ),
	kvsi_replace_vs( T, NxKvsi, OutKvsi ).
