div_list( [], _By, [] ).
div_list( [H|T], By, [Hdiv|Tdivs] ) :-
	Hdiv is H / By,
	div_list( T, By, Tdivs ).
