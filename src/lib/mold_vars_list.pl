mold_vars_list( [], [] ).
mold_vars_list( [_H|T], [_Fresh|FreshT] ) :-
	mold_vars_list( T, FreshT ).
