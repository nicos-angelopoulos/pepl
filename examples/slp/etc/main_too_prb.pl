:- multifile file_search_path/2.
file_search_path( slp, '../slp' ).

:- ensure_loaded( '../src/init_lib' ).
:- ensure_loaded( library('../sload_pe')  ).		% /1.
:- ensure_loaded( library('../estim') ).		% /4.
:- ensure_loaded( library(dbg) ).				% /1, switch_dbg/1.

:- switch_dbg( on ).

main :-
	sload_pe( too_probable ),
	estim( active1(_A,_B), [active1(m167780,alkylating)-1], Params,
		[count(exact),times(100),iter(5)] ),
	% sample_refutation( active1(A,B), 0.00000000000001, Path, Prb ),
	% write( ref(Prb,Path) ), nl.
	write( params(Params) ), nl.

main_sample :-
	main_gen( Results ),
	retractall( pp(_OldPP) ), 
	assert( pp([1-0.8,2-0.2,3-0.3,4-0.7,5-0.1,6-0.9]) ),
	write( pp([1-0.8,2-0.2,3-0.3,4-0.7,5-0.1,6-0.9]) ), nl,
	list_frequency( Results, FreqRes ),
	keysort( FreqRes, SortRes ),
	dbg( (write(sorted_results), nl,
		write_list_with_line_numbers(SortRes,1,"4"),nl) ),
	estim( s(_A,_B), SortRes, Params, 
		[termin([iter(3)]),times(1000),count(exact)] ),
	write( new_parameters( Params ) ), nl.

main_exact :-
	main_gen( Results ),
	retractall( pp(_OldPP) ), 
	assert( pp([1-0.8,2-0.2,3-0.3,4-0.7,5-0.1,6-0.9]) ),
	write( pp([1-0.8,2-0.2,3-0.3,4-0.7,5-0.1,6-0.9]) ), nl,
	list_frequency( Results, FreqRes ),
	keysort( FreqRes, SortRes ),
	dbg( (write(sorted_results), nl,
		write_list_with_line_numbers(SortRes,1,"4"),nl) ),
	estim( s(_A,_B), SortRes, Params, 
		[count(sample),termin([iter(3)]),times(1000)] ),
	write( new_parameters( Params ) ), nl.

main_gen( Obs ) :-
	sload_pe( jc_ml_pe ),
	experiment( [
		times( 1000 ),
		% clause( sample_refutation( s(X,Y), _P, _R ), s(X,Y) ),
		clause( scall( s(X,Y), -inf, sample, _P, true, _R ), s(X,Y) ),
		file( '/dev/null' ), filemode( append ),
		observables( Obs ),
		count_failures(no)
		], 
		_Results ).
