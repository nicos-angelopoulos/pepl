fam_setrand( Flag, Seeds ) :-
	( Flag == false ->
		Seeds = none
		;
		( Flag == true ->
			grab_random_seeds( R1, R2, R3 ),
			Seeds = rand(R1,R2,R3)
			;	
			Flag = Seeds
			% Flag = rand(R1,R2,R3)
		),
		setrand( Seeds )
	).
