:- ensure_loaded( library(lists) ).			% select/3.

% options_cohesion( Dom, Sub, Out ) :-
options_cohesion( [], Out, Out ).
options_cohesion( [H|T], Sub, [H|TOut] ) :-
     functor( H, HName, HArity ),
     functor( VarH, HName, HArity ),
	( select( VarH, Sub, RmSub ) ->
		true
		;
		RmSub = Sub,
		print_message( warning, 
			warning(options_cohesion/3,option(H),not_in(Sub)) )
	),
	options_cohesion( T, RmSub, TOut ).
		
