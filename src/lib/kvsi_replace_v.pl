kvsi_replace_v( [Int-Val|T], Key, ToVal, [Int-NwVal|R] ) :-
	( Int < Key ->
		NwVal = Val,
		kvsi_replace_v( T, Key, ToVal, R )
		;
		Int =:= Key,
		NwVal = ToVal,
		R = T
	).
