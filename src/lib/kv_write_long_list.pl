%
kv_write_long_list( List, HowMany ) :-
	kv_write_long_list( List, HowMany, HowMany, user_output, ' , ' ).

kv_write_long_list( List, HowMany, Out ) :-
	kv_write_long_list( List, HowMany, HowMany, Out, ' , ' ).

kv_write_long_list( List, HowMany, HowMany, Sep ) :-
	kv_write_long_list( List, HowMany, HowMany, user_output, Sep ).

kv_write_long_list( [], _Level, _Count, _Out, _Sep ).
kv_write_long_list( [Hk-Hv|T], ThisMany, HowMany, Out, Sep ) :-
	IHv is Hv,
	format( Out, "~t~G~3|:~20f", [Hk,IHv] ), 
	( HowMany < 2 -> 		% is HowMany =:= 1
		nl(Out), Next is ThisMany
		;
		( T == [] ->
			nl
			;
			write( Sep ),
			Next is HowMany - 1
		)
	),
	kv_write_long_list( T, ThisMany, Next, Out, Sep ).
