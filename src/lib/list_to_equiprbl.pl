list_to_equiprbl( List, Equi ) :-
	length( List, Length ),
	mould_constant_quotient( List, 1, Length, Equi ).

mould_constant_quotient( [], _, _, [] ).
mould_constant_quotient( [_H|T], Nom, Dnm, [Nom/Dnm|R] ) :-
	mould_constant_quotient( T, Nom, Dnm, R ).
