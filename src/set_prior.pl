:- ensure_loaded( library(kv_decompose) ).		% /3.
:- ensure_loaded( library(kv_div_vs_by) ).		% /3.
:- ensure_loaded( library(kvsi_replace_vs) ).	% /3.
:- ( predicate_property(between(_,_,_),built_in)  ->
          true
          ;
          ensure_loaded( library(between) )
   ). % /3.
:- ensure_loaded( library(lists) ).			% sum_list/2.

set_prior( InName ) :-
	( InName == random -> 
		ModName = random_prior 
		;
		( (InName == false;InName == none) ->
			ModName = no_explicit_prior_for_fam
			;
			ModName = InName
		)
	),
	findall( Beg-End, 
		spec_cids( _Spec, s, Beg, End ),
			Ranges ),
	bb_get( pp, PPs ),
	( ModName = Mod:Name ->
		Call = Mod:CallPred
		; 
          Name = ModName,
          Call = CallPred
		% Name = ModName, Mod = user
	),
	CallPred =.. [Name,Ranges,PPs,NwPPs],
	( call( Call ) ->
		bb_put( pp, NwPPs )
		;
		write( user_error, 'Call for setting priors failed.' ), nl( user_error ),
		write( user_error, 'Call was ' ), write( user_error, Call ),
		nl( user_error ),
		( Name == uniform -> 
			write( 'Aborting because uniform prior failed.' ), 
			nl( user_error ), abort
			;
			write( user_error, 'Using uniform distribution.' ), 
			nl( user_error ), set_prior( uniform )
		)
	),
	!.

% ALSO ADD RANDOM...
%
uniform( [], PPs, PPs ).
uniform( [Beg-End|T], PPs, NwPPs ) :-
	Length = End - Beg + 1,
	kvsi_replace_v_for_k_range( PPs, Beg, End, 1/Length, NxPPs ),
	uniform( T, NxPPs, NwPPs ).

random_prior( [], PPs, PPs ).
random_prior( [Beg-End|T], PPs, NwPPs ) :-
	findall( El-Rnd, (between(Beg,End,El),random(Rnd)), All ),
	kv_decompose( All, _Els, Rnds ),
	sum_list( Rnds, SumRnds ),
	kv_div_vs_by( All, SumRnds, NormAll ),
	kvsi_replace_vs( NormAll, PPs, NxPPs ), 
	random_prior( T, NxPPs, NwPPs ).

no_explicit_prior_for_fam( _, PPs, PPs ).

%%%

kvsi_replace_v_for_k_range( [Hk-Hv|T], Beg, End, Val, Rplcd ) :-
	( Hk < Beg ->
		Rplcd = [Hk-Hv|TRplcd],
		kvsi_replace_v_for_k_range( T, Beg, End, Val, TRplcd )
		;
		( Hk =:= Beg ->
			NwHv = Val
			;
			NwHv = Hv
		),
		( Beg < End ->
			Next is Beg + 1,
			Rplcd = [Hk-NwHv|TRplcd],
			kvsi_replace_v_for_k_range( T, Next, End, Val, TRplcd )
			;
			Rplcd = [Hk-NwHv|T]
		)
	).
