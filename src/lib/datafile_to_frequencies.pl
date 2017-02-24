:- ensure_loaded( library(lists) ). 		% append/3

:- requires( read_terms/2 ).
:- requires( list_frequency/2 ).
:- requires( break_list_on/4 ).
:- requires( kv_compose_fixed_v/3 ).

datafile_to_frequencies( File, Goal, Frqs ) :-
     read_terms( File, Terms ),
     Terms = [FirstTerm|_],
	( FirstTerm = frequencies( Frqs ) ->
		true
		;
          ( Terms = [data(_,_)|_] ->
               ( var(Goal) ->
                    write( user_error, on_mcmc_related_runs_option_is_required(goal/1) ), nl( user_error ), abort
                    ;
                    functor(Goal,GName,_)
               ),
               % From here on it is very specific to the MCMCMS/compare format
               dataterm_mcmcms_to_pe_frequencies( Terms, GName, Frqs )
               ;
		     ( break_list_on( Terms, (:-negative_examples), Ptv, Ngv) ->
			     list_frequency( Ptv, PtvFrqs ),
			     kv_compose_fixed_v( Ngv, 0, NgvFrqs ),
			     append( PtvFrqs, NgvFrqs, Frqs )
			     ;
			     list_frequency( Terms, Frqs )
		     )
          )
	).
	% dbg_ls( frequencies_are, Frqs ),
	% abort.

dataterm_mcmcms_to_pe_frequencies( [], _GName, [] ).
dataterm_mcmcms_to_pe_frequencies( [data(BnSample,HTms)|T], GName, [HF|TFs] ) :-
     HFk =.. [GName,BnSample],
     HF = HFk-HTms,
     dataterm_mcmcms_to_pe_frequencies( T, GName, TFs ).
