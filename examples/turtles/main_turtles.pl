:- multifile file_search_path/2.
file_search_path( slp, '../slp' ).

:- ensure_loaded( '../../prolog/pepl' ). 		% fam/1.
:- ensure_loaded( library(run_n_times) ).


main :-
	pepl:dbg_pepl( (write(running(main_exact)), nl) ),
	main_exact.

main_sample :-
	main_gen( Results ),
	% bb_put( pp, [1-0.25,2-0.25,3-0.25,4-0.25] ),
	pepl:list_frequency( Results, FreqRes ),
	keysort( FreqRes, SortRes ),
	pepl:dbg_pepl( (write(sorted_results), nl,
		write_list_with_line_numbers(SortRes,1,"4"),nl) ),
	fam( [ goal(sentence(_A, [])), data(SortRes), prior(uniform),
			eps(1.0e-4), count(sample), termin([iter(12)])] ).

main_exact :-
	main_gen( Results ),
	% bb_put( pp, [1-0.25,2-0.25,3-0.25,4-0.25] ),
	pepl:list_frequency( Results, FreqRes ),
	keysort( FreqRes, SortRes ),
	pepl:dbg_ls_pepl( sorted_results, SortRes ),
	fam( [ goal(sentence,_A,[]), data(SortRes), prior(uniform),
			eps(1.0e-4), count(exact), termin([iter(6)])
			] ).

main_store :-
	main_gen( Results ),
	% retractall(pp(_)),
	% bb_put(pp, [1-0.25,2-0.25,3-0.25,4-0.25]),
	pepl:list_frequency( Results, FreqRes ),
	keysort( FreqRes, SortRes ),
	pepl:dbg_ls_pepl( sorted_results, SortRes ),
	% goal(phrase(sentence(_A, _B),_Aagain,[]))
	fam( [ goal(sentence(_A, [])), data(SortRes), prior(uniform),
		  eps(1.0e-4), count(store), termin([iter(6)])
		  ] ).

% this does not work on 2.0.0
main_gen( Obs ) :-
% main_gen( Results ) :-
	sload_pe( turtles ),
	% spy( slp:s ),
     Goal = scall( sentence(A, []), 1.0e-3, sample, _P, true, _R ),
     AObs = sentence( A, [] ),
     Wrt  = false, 
     Fcnt = false,
     run_n_times( 1000, Goal, AObs, Wrt, Fcnt, _, Obs ).
     /*
	experiment( [
		times( 1000 ),
		% clause( sample_refutation( phrase(s,A,[]), _P, _R ), phrase(s,A,[]) ),
		clause( scall( sentence(A, []), 1.0e-3, sample, _P, true, _R ), sentence(A, []) ),
		file( '/dev/null' ), filemode( append ),
		observables( Obs ), count_failures(no)
		], 
		_Results ).
% 	list_frequency( Obs, FreqObs ).
*/
