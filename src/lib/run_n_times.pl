run_n_times( 0, _Goal, _Obs, _Bool, _FC, [], [] ) :-
	!.
run_n_times( Times, Goal, Obs, Bool, Fcount, Goals, Observed ) :-
	copy_term( [Goal,Obs], [FGoal,FObs] ),
	( call( FGoal ) ->
		LessByOne is Times - 1,
		Goals = [FGoal|TGoals],
		Observed   = [FObs|TObs]
		;
		( Fcount == false ->
			LessByOne is Times,
			Goals = TGoals,
			Observed   = TObs
			;
			LessByOne is Times - 1,
			Goals = [FGoal|TGoals],
			Observed   = [failed(FObs)|TObs]
		)
	),
	write_one_experiment_s_results( Bool, Times, FGoal, FObs ),
	run_n_times( LessByOne, Goal, Obs, Bool, Fcount, TGoals, TObs ).

write_one_experiment_s_results( false, _Tim, _Goal, _Obs ) :- !.
write_one_experiment_s_results( _, Tim, Goal, Obs ) :-
	format( "~t~d~6|", [Tim] ),  % six_space_numb( Tim ),
	write( ' : ' ), write( Goal ), write( ' : ' ),
	write( Obs ), nl.

% six_space_numb( Tim ) :-
	% pl( eclipse(_) ), % !, % printf( "%t%6d|", [Tim] ).
% six_space_numb( Tim ) :- format( "~t~d~6|", [Tim] ).
