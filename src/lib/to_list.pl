%% to_list( Term, Listed ).
%
% Wrap Term into a list of it not one already.
% For converting to lists see term_to_list/3.
%
%==
% ?- to_list( atom, List ).
% List = [atom].
%==
%
% @author nicos angelopoulos
% @version  0.2 2014/8/20
%
to_list( Either, List ) :-
	( (var(Either);(Either\=[_H|_T],Either\==[]) ) ->
		List = [Either]
		;
		List = Either
	).
