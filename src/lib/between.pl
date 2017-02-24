% between( +L, +H, ?N ) :-
% this may not be the most efficient implementation.
% This is a built-in, in swi (implemented in C i think).
between( L, H, N ) :-
	L =< H,
	between_1( L, H, N ).

between_1( L, _H, L ).
between_1( L, H, N ) :-
	L1 is L + 1,
	between( L1, H, N ).
