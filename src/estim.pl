:- ensure_loaded( myoption ).							% /5, option_sel/5.
:- ensure_loaded( library(lists) ).
	% select/3, memberchk/2, append/3.
:- ensure_loaded( library(ordsets) ).
	% list_to_ord_set/2, ord_union/3.
:- ensure_loaded( library(flatten_nv) ).
:- ensure_loaded( library(kv_write_long_list) ).			% /4.
:- ensure_loaded( library(kv_sum_vs) ).				     % /2,3.
:- ensure_loaded( library(kvsi_merge_w_add) ).			% /3.
:- ensure_loaded( library(kvis_val_left_incl) ).			% /4.
:- ensure_loaded( library(kv_replace_first_vals_w_rem) ).	% /4.
:- ensure_loaded( library(kv_div_vs_by) ).				% /3.
:- ensure_loaded( library(kv_multi_vs_by) ).				% /3.
:- ensure_loaded( library(kv_decompose) ).				% /3.
:- ensure_loaded( library(div_all_to_prbs) ).			% /3.
:- ensure_loaded( library(list_to_equiprbl) ).			% /3.
:- ensure_loaded( library(list_frequency) ).				% /2.
:- ensure_loaded( library(simplify_mults_in_quotient) ).	% /3.
:- ensure_loaded( library(fam_setrand) ).				% /2.
% :- ensure_loaded( library(dbg_pepl) ). % this is now in pepl.pl no need to load

/*  
-- General
These predicates implement an instance of the FAM algorithm, which
learns the parameters of an SLP to fit some learning data.

-- Global
Initial set of parameters are assumed in bb_get( pp, InParams ).

-- Variable Names
Cnt 		- Count Method, atom in {exact,sample,store}
Goal		- the top goal of which yields are instances.
Data		- list of Datum-Times, pairs where Datum is a yield, and Times is
            the times it occured in the learning data.
*/

estim( Goal, Data, Options, Params ) :-
	bb_get( pp, InParams ),
	% dbg_pepl( (write(first_pp(InParams) ),nl) ),
	option_sel( setrand, Options, false, RndOpts, SetRand ),
	fam_setrand( SetRand, _Seeds ),
	option_sel( times, RndOpts, 10000, TmsOpts, Tms ),
	option_sel( eps, TmsOpts, 1.0e-10, EpsOpts, Eps ),
	option_sel( termin, EpsOpts, [iter(10)], TmnOpts, Tmn ),
	option_sel( count, TmnOpts, sample, CntOpts, Cnt ),
	option_sel( corr, CntOpts, 1.0e-10, CorOpts, Cor ),
	% Cor is currently obsolete, instead we use 1/Tms^2
	option_sel( complement, CorOpts, none, CompOpts, Comp ),
	kv_sum_vs( Data, N ),
	( Goal = [_G|_Gs] -> Goals = Goal ; Goals = [Goal] ),
	% length( Data, Dpoints ),
	% Idx is - Dpoints - 1,
	pe_preprocess( Cnt, Data, Goal, Eps ),
	Invar = i(Goals, Data, N, Tms, Eps, Cor, Comp),
	% should also change the following occurance of Goal
	% to Goals (for count=store). Note for count=store
	% Goals is ignored in estim/7, so we are safe there.
	bb_get( fam_write_parameters, WWhich/WWhere ),
	( WWhich == none -> 
          true
          ;
	     write( WWhere, 'Initial parameters.' ), nl( WWhere ),
	     kv_write_long_list( InParams, 2, 2, WWhere, ',\t' )
     ),
     pl( swi(_), FstDummyLL = 1, FstDummyLL = -inf ), 
	estim( Cnt, InParams, Invar, 1, FstDummyLL, Tmn, Params, ERet ),
	% estim( Cnt, InParams, Invar, 1, -inf, Tmn, Params, ERet ),
	ERet = (Iter,Why,Ll),
	option_sel( return, CompOpts, [], _RetOpts, Ret ),
	( memberchk( iter(Iter), Ret ) ->
		true ; true ),
	( memberchk( termin(Why), Ret ) ->
		true ; true ),
	( memberchk( ll(Ll), Ret ) ->
		true ; true ),
	( memberchk( final_pps(Params), Ret ) ->
		true ; true ),
	( memberchk( initial_pps(EvalParams), Ret ) ->
		evaluate_parameters(InParams,EvalParams) ; true ),
	bb_put( pp, Params ).

% pe_preprocess( CntMethod, Data, Goal, Epsilon ) :-
% This is only needed for storing expressions CntMethod = store.
% pe_preprocess( sample, _Data, _Goal, _Eps ) :- !.
% pe_preprocess( exact, _Data, _Goal, _Eps ) :- !.
pe_preprocess( CntMethod, Data, Goal, Eps ) :-
	( CntMethod == store ->
		bb_get( pp, Params ),
		mold_vars_list( Params, Labels ),
		length( Data, Yields ),
		store_psi_y_k( Data, Params, Labels, Eps, 1, 0, Yields, SumExp ),
		clean_summation( SumExp, ClSumExp ),
		length( Params, M ),
		Offset is M * Yields + 1,
		% Idx is - Yields - 1,
		bb_put( psi_true, (Labels,ClSumExp) ),
		dbg_pepl( (write(bb_put( psi_true, (Labels,ClSumExp))),nl) ),
		store_psi_fail( Goal, Eps, Params, Labels, Offset ),
		gather_sums_yikr( Params, 1, M, Data, Yields, Offset )
		;
		true
	).

%%% begin store preprocess preds
gather_sums_yikr( [], _I, _M, _Data, _N, _Off ).
gather_sums_yikr( [_H|T], I, M, Data, N, Off ) :-
	gather_sum_yikr( Data, 1, N, I, Vars, Sum ),
	clean_summation( Sum, CleanS ),
	FIdx is Off + I - 1,
	bb_get( FIdx, (Vars,FiExpr) ),
	clean_summation( FiExpr, CleanFE ),
	bb_get( psi_fail, (Vars,PsiFail) ),
	( CleanFE == 0 -> 
		PsiNu_iYExp = CleanS
		;
		simplify_mults_in_quotient( CleanFE, PsiFail, Quotient ),
		dbg_pepl( (write( 
		  simplify_mults_in_quotient( CleanFE, PsiFail, Quotient )
		  	),nl) ),
		( Quotient == 1 ->
			PsiNu_iYExp = CleanS + Nz 
			;
			PsiNu_iYExp = CleanS + Nz * Quotient
		)
	),
	bb_put( I, (Nz,Vars,PsiNu_iYExp) ),
	dbg_pepl( (write( bb_put(I,(Nz,Vars,PsiNu_iYExp)) ), nl) ),
	NxI is I + 1,
	gather_sums_yikr( T, NxI, M, Data, N, Off ).

gather_sum_yikr( [_Yk-Tms|Data], K, N, I, Vars, SumTerm ) :-
	Idx is (I-1) * N + K,
	bb_delete( Idx, (Vars,Term) ),
	dbg_pepl( (write( bb_delete(Idx,(Vars,Term)) ),nl) ),
	NegK is - K,
	bb_get( NegK, (Vars,Denom) ),
	dbg_pepl( (write( bb_get( NegK, (Vars,Denom) ) ),nl) ),
	( \+ \+ Term = Denom -> Quotient is 1 ; 
		simplify_mults_in_quotient( Term, Denom, Quotient ),
		dbg_pepl( (write(simplify_mults_in_quotient(Term,Denom,Quotient)),nl) )
	),
	( K =:=  N -> 
		( Term == 0 -> SumTerm = 0 ; 
			if_ground_eval_else_unify( Tms * Quotient, SumTerm ) )
		;
		( Term == 0 ->
			NxK is K + 1,
			gather_sum_yikr( Data, NxK, N, I, Vars, RightSum ),
			if_ground_eval_else_unify( RightSum, SumTerm )
			;
			if_ground_eval_else_unify( Tms * Quotient, LSumTerm ),
			NxK is K + 1,
			gather_sum_yikr( Data, NxK, N, I, Vars, RSum ),
			if_ground_eval_else_unify( RSum, RSumTerm ),
			if_ground_eval_else_unify( LSumTerm + RSumTerm, SumTerm )
		)
	).

if_ground_eval_else_unify( Expression, EvalOr ) :-
	( ground(Expression) ->
		EvalOr is Expression
		;
		EvalOr = Expression
	).

clean_summation( Sum, Clean ) :-
	( var(Sum) -> 
		Clean = Sum 
		;
		( Sum = A + B -> 
				clean_summation( A, CleanA ),
				clean_summation( B, CleanB ),
				sum_expr_simplify( CleanA, CleanB, Clean )
				;
				Clean = Sum 
		)
	).

sum_expr_simplify( A, B, AplusB ) :-
	( A == 0 ->
		AplusB = B
		;
		( B == 0 -> 
			AplusB = A
			;
			AplusB = A + B
		) 
	).

% store preprocess predicates...
store_psi_fail( Goal, Eps, Params, Labels, KPlus ) :-
     write( user_error, g(Goal) ), nl( user_error ),
	findall( Flure,
			(scall(Goal,Eps,all,Path,Succ,_BrFlPrb),Succ==fail,
				flatten_nv(Path,Flure),
                    % HERE
				write( flatten_nv(Path,Flure) ), nl
                    ),
		 Flures ),
	( Flures = [] ->
		PathExprs = [], PsiFail = 0
		;
		remove_duplicates( Flures, PrunedFlures ),
		paths_to_expressions( PrunedFlures, Labels, PathExprs, PsiFail )
	),
	% Idx is - KPlus,
	bb_put( psi_fail, (Labels,PsiFail) ),
	dbg_pepl( (write( bb_put( psi_fail, (Labels,PsiFail) ) ),nl) ),
	store_psi_is_k( Params, 1, Labels,  PathExprs, KPlus, 1 ).

store_psi_y_k( [], _Ps, _Lbs, _Eps, _K, SumExps, _Dps, SumExps ).
store_psi_y_k( [Datum-_Tms|Freq], Params, Labels, Eps, K, AccExp, Dps, SumExps ) :-
	findall( FPath,
			(scall(Datum,Eps,all,EPath,Succ,_Prb),Succ\==fail,
				flatten_nv(EPath,FPath),
				write( flatten_nv(EPath,FPath) ), nl
                    ),
		Paths ),
	remove_duplicates( Paths, PrunedPaths ),
	paths_to_expressions( PrunedPaths, Labels, PathExprs, PsiY_k ),
	store_psi_is_k( Params, 1, Labels, PathExprs, K, Dps ),
	Idx is - K,
	bb_put( Idx, (Labels,PsiY_k) ),
	NxAccExp = AccExp + PsiY_k,
	dbg_pepl( (write( bb_put(Idx,(Labels,PsiY_k)) ),nl) ),
	NxK is K + 1,
	store_psi_y_k( Freq, Params, Labels, Eps, NxK, NxAccExp, Dps, SumExps ).

store_psi_is_k( [], _I, _VarsSet,  _PathExprs, _K, _N_k ).
store_psi_is_k( [Id-_Lbl|TPrms], I, VarsSet, PathExprs, K, N_k ) :-
	storable_psi_i_k( PathExprs, Id, Sum ),
	Ik is (I-1) * N_k + K,
	dbg_pepl( (write(Ik is (I-1)*N_k+K),nl) ),
	clean_summation( Sum, Clean ),
	bb_put( Ik, (VarsSet,Clean) ),
	dbg_pepl( (write( bb_put( Ik, (VarsSet,Clean) ) ),nl) ),
	NxI is I + 1,
	store_psi_is_k( TPrms, NxI, VarsSet, PathExprs, K, N_k  ).
	
storable_psi_i_k( [], _I, 0 ).
storable_psi_i_k( [PsiY_kjExp - FreqH|TPEs], I, Sum  ) :-
	( TPEs == [] -> 
		( memberchk(I-ITms,FreqH) -> 
			( ITms =:= 1 ->
				Sum = PsiY_kjExp
				;
				Sum = PsiY_kjExp * ITms
			)
			;
			Sum is 0
		)
		;
		( memberchk(I-ITms,FreqH) -> 
			( ITms =:= 1 ->
				Sum = PsiY_kjExp + RightSum
				;
				Sum = PsiY_kjExp * ITms + RightSum
			)
			;
			Sum = RightSum
		),
		storable_psi_i_k( TPEs, I, RightSum  )
	).

paths_to_expressions( [], _Vars, [], 0 ) :-  % well, it depends.
	write( 'Error: Empty Paths, in paths_to_expressions/4.' ),
	nl, abort.
paths_to_expressions( [H|T], Vars, [Hex|Texps], Summation ) :-
	Hex = PsiY_kjExp - FreqH,
	flat_path_to_integers( H, IntH ),
	list_frequency( IntH, FreqH ),
	freq_path_to_expressions( FreqH, Vars, PsiY_kjExp ),
	( T = [] -> 
		Summation = PsiY_kjExp,
		Texps = []
		;
		Summation = PsiY_kjExp + RemSum,
		paths_to_expressions( T, Vars, Texps, RemSum )
	).

flat_path_to_integers( [], [] ).
flat_path_to_integers( [H|T], Ints ) :-
	( (integer(H),H>0) -> 
		Ints = [H|Tints]
		;
		Ints = Tints
	),
	flat_path_to_integers( T, Tints ).

% freq_path_to_expressions( [], _Vars, [], 0 ).
% freq_path_to_expressions( FreqH, Vars, PsiR_kjExp  ),
freq_path_to_expressions( [C_i-C_iTms|T], Vars, PsiR_kj ) :-
	bb_get( pp, PP ),
	nth( Nth, PP, C_i-_Hval ),
	nth( Nth, Vars, NthVar ),
	!,
	( T = [] -> 
		( C_iTms =:= 1 -> 
			PsiR_kj = NthVar
			;
			PsiR_kj = NthVar ** C_iTms
		)
		;
		( C_iTms =:= 1 -> 
			PsiR_kj = NthVar * RemPsiR_kj
			;
			PsiR_kj = NthVar ** C_iTms * RemPsiR_kj
		),
		freq_path_to_expressions( T, Vars, RemPsiR_kj )
	).

%%% end store preprocess preds.

% estim( CntMethod, InPrms, Invar, I, PrvLl, Tmn, OutPrms ) :-
% main loop construct. Go around getting nxt_parameters,
% until termination. 
% Invar is a tuple of invariants, used in acquiring next parameters.
% PrvLl = PrvLl, previous loglikelyhood. Tmn = Termination list of terms.
estim( done, Prms, _, _, _, _, Prms, _ ) :- !.
estim( Cnt, InPrms, Invar, I, PrvLl, TCond, OutPrms, ERet ) :-
	nxt_params( Cnt, InPrms, Invar, InLl, NxtPrms ),
	!, % most likely superfluous, added just in case it helps performance
	garbage_collect,
	( terminate(InPrms, I, PrvLl, InLl, NxtPrms, TCond, Why ) -> 
		bb_get( fam_write_parameters, WWhich/WWhere ),
		( WWhich == ends -> 
			write( WWhere, 'Last Iteration'(I) ), 
               write( WWhere, '.' ), nl( WWhere ),
			kv_write_long_list( NxtPrms, 2, 2, WWhere, ',\t' )
			;
			true
		),
 		% write( log_likelihood(InLl) ), nl, 
		OutPrms = NxtPrms,
		ERet = (I,Why,InLl),
		NxCnt = done
		;
		NxCnt = Cnt,
		NxtI is I + 1
	),
	estim( NxCnt, NxtPrms, Invar, NxtI, InLl, TCond, OutPrms, ERet ).

% nxt_params( +Cnt, InPParams, (_Goal, Data, N, _Tms, _Eps, _Cor), Ll, OutPParams ) :-
nxt_params( store, InPParams, i(_G,Data,N,_Ts,_Ep,_Cr,Cmp), Ll, OutPParams ) :-
	!,
	kv_decompose( InPParams, _Is, Prms ),
     prbsc_store( Cmp, Prms, PrbSc ),
	%% 2005bb09 PrbSc is PsiTrue / (PsiTrue + PsiFail),
	dbg_pepl( (write( prob_of_succ(PrbSc) ), nl) ),
	Nz is N * ( 1 / PrbSc - 1 ),
	dbg_pepl( (write( nz(Nz) ), nl) ),
	% length( InPParams, M ),
	store_bb_to_cis( InPParams, 1, Nz, Prms, Cis ),
	dbg_ls_pepl( cis, Cis ),
	store_log_likelihood( Data, -1, Prms, PrbSc, 0, Ll ),
 	write_ll( Ll ),
	new_params( Cis, [], OutPParams ).

nxt_params( Cnt, InParams, i(Goal, Data, N, Tms, Eps, _Cr,Cmp), Ll, OutParams ) :-
	bb_put( pp, InParams ),
	path_counts( Cnt, Goal, Eps, Tms, Data, Cmp, ScPrb, DaTiCoRu, FlFrq ),
	dbg_ls_pepl( daticoru, DaTiCoRu ),
	dbg_ls_pepl( failure_frequency, FlFrq ),
	% dbg_pepl( (write(number_of_failures(FlTms)),nl) ),
	% length( Flures, Nof ), write( nof(Nof) ), nl,
	% clauses_freqs_in_paths( Flures, [], 0, FailCloccs ),
	CurrCorr is 1 / (Tms * Tms),
	log_likelihood( Data, DaTiCoRu, ScPrb, CurrCorr,  0, Ll ),
	% log_likelihood( Data, DaTiCoRu, Cor,  0, Ll ),
	% log_likelihood( DaTiCoRu, SuccProb, 0, Ll ),
	% dbg_pepl( (write(exact_failures(FailCloccs)),nl) ),
	dbg_pepl( (write(data_size(N)),nl) ),
	dbg_pepl( (write(probability_of_success(ScPrb)),nl) ),
	Cf is N * ((1/ScPrb)-1),
	dbg_pepl( (write(constant_of_failure(Cf)),nl) ),
	cis( InParams, DaTiCoRu, Cf, FlFrq, Cis ),
	dbg_ls_pepl( cis, Cis ),
	% dbg_pepl( (write( log_likelihood(Ll)), nl) ),
 	write_ll( Ll ),
	% dbg_ls_pepl( cis, Cis ),
	new_params( Cis, [], OutParams ).

prbsc_store( none, Prms, PrbSc ) :-
     !,
	bb_get( psi_true, (Prms,PrbTrue) ),
     PrbSc is PrbTrue,
	write( user_error, prbsc(PrbSc) ), nl( user_error ).
prbsc_store( success, Prms, PrbSc ) :-
     !,
	bb_get( psi_fail, (Prms,PsiFail) ),
	dbg_pepl( (write(bb_get(psi_fail,(Prms,PsiFail))),nl) ),
	PrbSc is 1 - PsiFail.
prbsc_store( quotient, Prms, PrbSc ) :-
	bb_get( psi_fail, (Prms,PsiFail) ),
	bb_get( psi_true, (Prms,PrbTrue) ),
     CompPrbTrue is PrbTrue,
	PrbSc is CompPrbTrue / (CompPrbTrue + PsiFail),
	dbg_pepl( (CompPsiFail is PsiFail, write((PrbSc is CompPrbTrue / (CompPrbTrue + CompPsiFail))),nl) ).

store_log_likelihood( [], _K, _Prms, _PrbSc, Ll, Ll ).
store_log_likelihood( [_Y_k-Nk|Data], K, Prms, PrbSc, Acc, Ll ) :-
	bb_get( K, (Prms,PsiYk) ),
	NxAcc is Acc + Nk * log(PsiYk / PrbSc),
	NxK is K - 1,
	store_log_likelihood( Data, NxK, Prms, PrbSc, NxAcc, Ll ).

log_likelihood( [], _DaTiCoRu, _PrbSc, _Corr, Ll, Ll ).
log_likelihood( [Yk-YNk|T], DaTiCoRu, PrbSc, Corr, AcLl, Ll ) :-
	( (select(Yk-Nk-_Co-YkTrue,DaTiCoRu,NxDaTiCoRu),
	   YkTrue > 0 ) ->
		NxLl is AcLl + ( Nk * log(YkTrue/PrbSc) )
		% write( NxLl is AcLl + ( Nk * log(YkTrue/PrbSc) ) ), nl
		;
		dbg_pepl( (write(no_occurances_of(Yk)),nl) ),
		% check this with James
		NxLl is AcLl + (YNk * log(Corr)),
		dbg_pepl( (NxLl is AcLl + (YNk * log(Corr)),nl) ),
		NxDaTiCoRu = DaTiCoRu
	),
	log_likelihood( T, NxDaTiCoRu, PrbSc, Corr, NxLl, Ll ).

% cis( , DataCounts, Cf, FlFrq,  ) :-
cis( [], _DataCounts, _Cf, _FlFrq, [] ).
cis( [ClId-_Cpar|T], DataCounts, Cf, FlFrq, [ClId-ClCi|Tcis]  ) :-
	cis_clause( DataCounts, ClId, 0, ClSum ),
	( memberchk(ClId-Cnts,FlFrq) -> 
		ClCi is ClSum + ( Cf * Cnts )
		% dbg_pepl( (write((ci(ClId : ClCi is ClSum + ( Cf * Cnts )))),nl) )
		;
		% dbg_pepl( (write((ci(ClId : ClSum))),nl) ),
		ClCi = ClSum
	),
	cis( T, DataCounts, Cf, FlFrq, Tcis ).

cis_clause( [], _ClId, ClSum, ClSum ).
cis_clause( [_Obs-Tms-CiConds-_TotPrb|T], ClId, Acc, ClCi ) :-
	( memberchk(ClId-ClCiCond,CiConds) ->
		NxAcc is Acc + (Tms*ClCiCond)
		% dbg_pepl( (write(acc_sum(ClId : NxAcc is Acc + (Tms*ClCiCond))),nl) )
		;
		NxAcc = Acc
		% , dbg_pepl( (write(acc_sum_no(ClId : NxAcc)),nl) )
	),
	cis_clause( T, ClId, NxAcc, ClCi ).

new_params( [], NewParams, NewParams ).
%% NOW (2005/03/01) should change to: new_params( [H|T], Acc, OutParams ) :-
new_params( [HId-HCi|T], Acc, OutParams ) :-
	spec_cids( _Spec, s, HId, EndId ), !,
	%% kvis_val_left_incl( T, EndId, _EndVal, BrVals ),
	%% sum_list( [HCi|BrVals], Sum ),
	%% div_all_to_prbs( [HCi|BrVals], Sum, NormVals ),
	kvis_val_left_incl( [HId-HCi|T], EndId, _EndVal, BrVals ),
	sum_list( BrVals, Sum ),
	( Sum =:= 0 -> 
		% write( user_error, warning( noclauses_used_in([HId-HCi|T])) ), 
		list_to_equiprbl( BrVals, NormVals )
		;
		div_all_to_prbs( BrVals, Sum, NormVals )
	),
	kv_replace_first_vals_w_rem( NormVals, [HId-HCi|T], LOPs, RemT ),
	append( Acc, LOPs, NxAcc ),
	new_params( RemT, NxAcc, OutParams ).

% DaTiCoRu = [Datum-DaTms-ExpPrs-PsiY_k|TDaTiCoRus]
% Datum - the kth yield (y_k).
% DaTms - the times Datum appears in the data.
% ExpPrs - a list of Id-Psi[Nu_i|y_k], for all i's such that
%			clause i (C_i) appears in some r in R_k.
% PsiY_k - Psi(Y_k) = Sum_r Psi(r) for all r in R_k

path_counts( strict, _Goal, Eps, _Tms, Data, Comp, PrbSc, DaTiCoRu, CisNu_iFail ) :-
	!, % exact + strict way of calculating probability of failures.
	strict_daticoru( Data, 0, Eps, [], Comp, PrbTrue, DaTiCoRu, PrunedPrFlures ),
	% write( prf(PrunedFlures) ), nl, abort,
	% findall( Flure-BrFlPrb, 
			% (	member( Yk-_YkTms, Data ),
				% scall(Yk,Eps,all,Path,Succ,BrFlPrb),Succ==fail,
				% flatten(Path,Flure)),
		 % PrFlures ),
	% remove_duplicates( PrFlures, PrunedPrFlures ),
	kv_decompose( PrunedPrFlures, _Flures, BrsFlPrbs ),
	% dbg_ls_pepl( pruned_branch_failures, PrunedPrFlures ),
     prbsc_counts( Comp, PrbTrue, BrsFlPrbs, PrbSc ),
	dbg_pepl( (write(succ_prb(PrbSc)),nl) ),
	% exact_daticoyk_to_daticoru( DaTiCoYk, ScPrb, DaTiCoRu ),
	% length( PrFlures, NoOfFls ),
	paths_to_cond_cis( PrunedPrFlures, [], 0, _SumCiFail, CisNu_iFail ).
	% paths_to_cond_cis( PrFlures, [], 0, _SumCiFail, CisNu_iFail ).
	% paths_to_clausal_freqs( Flures, [], 0, FlFrq ).

path_counts( exact, Goal, Eps, _Tms, Data, Comp, PrbSc, DaTiCoRu, CisNu_iFail ) :-
	!,
	% exact_daticoru(  Data, 0, Eps, ScPrb, DaTiCoYk ),
	exact_daticoru(  Data, 0, Eps, PrbTrue, DaTiCoRu ),
	% HERE TMP ONLY goals_to_failure_pairs( Goal, Eps, [], PrFlures ),
	%% PrFlures = [],
     prbsc_exact( Comp, Goal/Eps, PrbTrue, PrbSc, CisNu_iFail ).
	%% dbg_pepl( (write(Z is ScPrb / (ScPrb + FlPrb)),nl) ),
	% dbg_pepl( (write(succ_prb(ScPrb)),nl) ),
	% exact_daticoyk_to_daticoru( DaTiCoYk, ScPrb, DaTiCoRu ),
	% length( PrFlures, NoOfFls ),
	% paths_to_clausal_freqs( Flures, [], 0, FlFrq ).

path_counts( sample, Goals, Eps, Tms, Data, _Comp, ScPrb, DaTiCoRu, CisNu_iFail ) :-
     % we are currently ignore Comp(lement) here. 
     % ScPrb is always quotient of the sampled path prbs (succ + fails)
	count_sample_goals( Goals, Tms, Eps, Obs, [] ),
     %% write( count_sample_goals( Goals, Tms, Eps, Obs, [] ) ), nl,
     % list_frequency( PrvObs, Obs ),
	%%%% count_sample_goals( Goals, Tms, Eps, [], Obs ),
     % open('rep.pl', append, Out), write(Out,obs(Obs)), nl(Out), close(Out),
	sample_daticoru( Obs, Data, 0, 0, [], ScTms, FlTms, DaTiCoRu, PrFlures ),
     % write(fltms(FlTms)), nl,
	( FlTms =:= 0 ->
		ScPrb is 1, dbg_pepl( (write(ScPrb is one(1)),nl) )
		;
		ScPrb is ScTms / (FlTms + ScTms),
		dbg_pepl( (write(ScPrb is ScTms / (FlTms + ScTms)),nl) )
	),
	paths_to_cond_cis( PrFlures, [], 0, _SumCiFail, CisNu_iFail ).
	% paths_to_clausal_freqs( Flures, [], 0, FlFrq ).
	% list_frequency( Obs, Freqs ),
	% frequency_paths( Freqs, Paths )
     
prbsc_counts( none, PrbTrue, _BrsFlPrbs, PrbSc ) :-
     !,
     PrbSc = PrbTrue,
     dbg_pepl( (write(PrbSc = PrbTrue),nl) ).
prbsc_counts( success, _PrbTrue, BrsFlPrbs, PrbSc ) :-
     !,
	sum_list( BrsFlPrbs, PrbFl ),
	PrbSc is 1 - PrbFl,
	dbg_pepl( (write((PrbSc is 1 - PrbFl)),nl) ).
prbsc_counts( quotient, PrbTrue, BrsFlPrbs, PrbSc ) :-
	sum_list( BrsFlPrbs, PrbFl ),
	PrbSc is PrbTrue / (PrbTrue + PrbFl),
	dbg_pepl( (write((PrbSc is PrbTrue / (PrbTrue + PrbFl))),nl) ).

% prbsc_exact( none, _Goal, PrbTrue, PrbSc, [] ) :-
prbsc_exact( skip_failures, _Goal, PrbTrue, PrbSc, [] ) :-
     !,
     PrbSc = PrbTrue,
     dbg_pepl( (write(PrbSc = PrbTrue),nl) ).
prbsc_exact( none, Goal/Eps, PrbTrue, PrbSc, CisNu_iFail  ) :-
     !,
     PrbSc = PrbTrue,
	goals_to_failure_pairs( Goal, Eps, [], PrFlures ),
	paths_to_cond_cis( PrFlures, [], 0, _SumCiFail, CisNu_iFail ),
     dbg_pepl( (write(PrbSc = PrbTrue),nl) ).
prbsc_exact( success, Goal/Eps, _PrbTrue, PrbSc, CisNu_iFail ) :-
     !,
	goals_to_failure_pairs( Goal, Eps, [], PrFlures ),
	remove_duplicates( PrFlures, PrunedPrFlures ),
	paths_to_cond_cis( PrFlures, [], 0, _SumCiFail, CisNu_iFail ),
	kv_decompose( PrunedPrFlures, _Flures, BrsFlPrbs ),
	sum_list( BrsFlPrbs, PrbFail ),
	%%% pre 2005bb09 : Z is ScPrb / (ScPrb + FlPrb),
	( (PrbFail =< 1.0,PrbFail >= 0) -> 
		true
		;
		write( user_error, flprb_out_of_range(PrbFail) ), nl( user_error ),
		abort
	),
	% HERE  JC says Z = ScPrb. % 2005bb10
	PrbSc is 1 - PrbFail,
	dbg_pepl( (write(PrbSc is 1 - fl_prb(PrbFail)), nl) ).
prbsc_exact( quotient, Goal/Eps, PrbTrue, PrbSc, CisNu_iFail ) :-
	goals_to_failure_pairs( Goal, Eps, [], PrFlures ),
	remove_duplicates( PrFlures, PrunedPrFlures ),
	kv_decompose( PrunedPrFlures, _Flures, BrsFlPrbs ),
	paths_to_cond_cis( PrFlures, [], 0, _SumCiFail, CisNu_iFail ),
	sum_list( BrsFlPrbs, PrbFail ),
	PrbSc is PrbTrue / (PrbTrue + PrbFail),
	dbg_pepl( (write(prbsc(PrbSc is PrbTrue / (PrbTrue + PrbFail))),nl) ).

count_sample_goals( [], _Tms, _Eps, Obs, Obs ).
count_sample_goals( [G|Gs], Tms, Eps, Obs, AllObs ) :-
     count_sample_tms_goal( Tms, G, Eps, Obs, NxtObs ),
     count_sample_goals( Gs, Tms, Eps, NxtObs, AllObs ).

count_sample_tms_goal( 0, _G, _Eps, TailObs, TailObs ) :- !.
count_sample_tms_goal( Tms, G, Eps, [Obs|TObs], NxtObs ) :-
     copy_term(G, FreshG),
     scall(FreshG,Eps,sample,Path,Succ,Prb),
     % HERE should be a more efficient way than storing the Goal
     % try N = Nx + 1..... (might be Nk elsewhere anyways).
     Obs = FreshG-Succ-Path-Prb,
     NxtTms is Tms - 1,
     count_sample_tms_goal( NxtTms, G, Eps, TObs, NxtObs ).

goals_to_failure_pairs( [], _Eps, Pairs, Pairs ).
goals_to_failure_pairs( [H|T], Eps, AccPairs, Pairs ) :-
	findall( Flure-BrFlPrb, 
			(scall(H,Eps,all,Path,Succ,BrFlPrb),Succ==fail,
				flatten_nv(Path,Flure)
				%, write( flatten(Path,Flure) ), nl
                    ),
		 PrFlures ),
	append( AccPairs, PrFlures, NxPairs ),
	goals_to_failure_pairs( T, Eps, NxPairs, Pairs ).

%%% sample predicates
sample_daticoru( [], _Data, ScTms, FlTms, AcD, ScTms, FlTms, MrgD, [] ) :-
	merge_sampled_freqs( AcD, ScTms, MrgD ).
sample_daticoru( [Gk-Tr-Ph-Prb|T], Data, AcS, AcF, AcD, STms, FTms, DaTiCoRu, Fls ) :-
     % write( h(Gk-Tr-Ph-Prb) ), nl,
	( Tr == fail -> 
		NxF is AcF + 1,
		% NxF is AcF + Prb,
		NxS is AcS, 
		NxD = AcD,
		flatten_failure_path( Ph, [], FlatPh ),
		Fls = [FlatPh-Prb|TFls]
		;
		NxF is AcF,
		% NxS is AcS + Prb,
		NxS is AcS + 1,
		Fls = TFls,
		flatten_nv( Ph, FlatPath ),
		list_frequency( FlatPath, NwFreq ),
		keysort( NwFreq, NwSFrq ),
		( select(Gk-Nk-Fqs-CardRk,AcD,RmD) ->
			NxCardRk is CardRk + 1,
			NxD = [Gk-Nk-[NwSFrq|Fqs]-NxCardRk|RmD]
			;
			( memberchk(Gk-Nk,Data) ->
					NxD = [Gk-Nk-[NwSFrq]-1|AcD]
					;
					NxD = AcD
			)
		)
	),
	sample_daticoru( T, Data, NxS, NxF, NxD, STms, FTms, DaTiCoRu, TFls ).

merge_sampled_freqs( [], _ScTms, [] ).
merge_sampled_freqs( [Yk-Nk-MultFrq-CRk|T], ScTms, [Yk-Nk-PrbFrq-Ru|TMrgD] ) :-
	kvsi_multi_merge_w_add( MultFrq, UniFrq ),
 	length( MultFrq, AppTms ),
	kv_div_vs_by( UniFrq, AppTms, PrbFrq ),
	Ru is CRk / ScTms,
	merge_sampled_freqs( T, ScTms, TMrgD ).
	
%%% end sample predicates

%%% stored expressions predicates
store_bb_to_cis( [], _I, _Nz, _Lbls, [] ).
store_bb_to_cis( [Id-_Lbl|TPrms], I, Nz, Lbls, Cis ) :-
	bb_get( I, (Nz,Lbls,Expr) ),
	% dbg_pepl( (write((ci(Id : _ is Expr))),nl) ),
	Value is Expr,
	Cis = [Id-Value|TCis],
	NxI is I + 1,
	store_bb_to_cis( TPrms, NxI, Nz, Lbls, TCis ).
%%% stored expressions preds end
	
% this  is very similar to exact. i copied it here to make things clearer
% while developing.
strict_daticoru( [], CurrSum, _E, Flrs, CurrSum, [], Flrs ).
strict_daticoru( [Yk-YkTms|Dprs], AccS, E, AccFlrs, Sum, [Yk-YkTms-CondCis-CiYk|Paths], Flrs ) :-
	% write( finding( Yk ) ), nl,
	findall( Succ-FPath-Prb, 
			(scall(Yk,E,all,EPath,Succ,Prb), % Succ\==fail,
				flatten_nv(EPath,FPath)
                    % write( user_error, flatten(EPath,FPath) )
			% ( Succ\==fail ->
			% ( (Yk=localization('G234141',nucleus),Succ\==fail) ->
				 % write(Succ-FPath), nl, (FPath=[_,_|_]->abort;true) ; true )
				),
		AllSccPathsPrbs ),
	untangle_scall_paths( AllSccPathsPrbs, AllPathsPrbs, FlrsPrbPath ),
	list_to_ord_set( FlrsPrbPath, SetFlrsPrbPath ),
	ord_union( SetFlrsPrbPath, AccFlrs, NxAccFlrs ),
	remove_duplicates( AllPathsPrbs, PrunedPathsPrbs ),
	paths_to_cond_cis( PrunedPathsPrbs, [], 0, CiYk, CondCis ),
	NxSum is AccS + CiYk,
	strict_daticoru( Dprs, NxSum, E, NxAccFlrs, Sum, Paths, Flrs ).

untangle_scall_paths( [], [], [] ).
untangle_scall_paths( [S-Ph-Pb|T], Succs, Flrs ) :-
	( S == fail -> 
		Succs = TSuccs,
		Flrs  = [Ph-Pb|TFlrs]
		;
		Succs = [Ph-Pb|TSuccs],
		Flrs  = TFlrs

	),
	untangle_scall_paths( T, TSuccs, TFlrs ).

%%% exact predicates
exact_daticoru( [], CurrSum, _E, CurrSum, [] ).
exact_daticoru( [Yk-YkTms|Dprs], AccS, E, Sum, [Yk-YkTms-CondCis-CiYk|Paths] ) :-
	% write( user_error, scall(Yk,E,all,EPath,Succ,Prb) ), 
	findall( FPath-Prb, 
			(scall(Yk,E,all,EPath,Succ,Prb),Succ\==fail,
				flatten_nv(EPath,FPath)),
		AllPathsPrbs ),
	% write( user_error, '... done.' ),
	% nl( user_error ),
	remove_duplicates( AllPathsPrbs, PrunedPathsPrbs ),
	paths_to_cond_cis( PrunedPathsPrbs, [], 0, CiYk, CondCis ),
	NxSum is AccS + CiYk,
	exact_daticoru( Dprs, NxSum, E, Sum, Paths ).

% paths_to_cond_cis( PathsPrbs, WCCisAcc, AccSum, Sum, CondCis ) :-
% PathsPrbs (=[Path-PathPrb|Ps]),  WCCisAcc is the CodCis 
% unormalised asscumulator, and AccSum for the summed accumulation of Sum.
% CondCis is an indexed list ([Ci-Val|T) where Ci is a clause id and Val is
% the ci[n_i|y_k] terms of the second summation for the FAM equation.
% (k corresponds to current yield of Paths).
% This is used twice, once for x in X(y_k)  and once for x in Failures.
paths_to_cond_cis( [], AccWCCis, CiOfX, CiOfX, CondCis ) :-
	( CiOfX =:=  0 ->
		CondCis = []
		; 
		kv_div_vs_by( AccWCCis, CiOfX, CondCis )
	).
	% kv_div_vs_by( AccWCCis, CiY_k, CondCis ).
paths_to_cond_cis( [Path-Prb|T], AccWCCis, AccSumPrb, CiY_k, CondCis ) :-
	NxSumPrb is AccSumPrb + Prb,
	% flatten( Path, FlatPath ),
	list_frequency( Path, FreqPath ),
	% write( list_frequency( Path, FreqPath ) ), nl,
	keysort( FreqPath, SortPath ),
	kv_multi_vs_by( SortPath, Prb, MultPath ),
	kvsi_merge_w_add( MultPath, AccWCCis, NxAcc ),
	paths_to_cond_cis( T, NxAcc, NxSumPrb, CiY_k, CondCis ).

list_to_occurs_kv( [], [] ).
list_to_occurs_kv( [H|T], [H-1|R] ) :-
	delete( T, H, M ),
	list_to_occurs_kv( M, R ).

exact_daticoyk_to_daticoru( [], _ScPrb, [] ).
exact_daticoyk_to_daticoru( [D-T-C-Yk|Tail], ScPrb, DTCRs ) :-
	% [D-T-C-Ru|Tru] ) :-
	% check this with James
	( Yk = 0 ->
		Tru = DTCRs
		;
		Ru is Yk / ScPrb,
		dbg_pepl( (write( (Ru is Yk / ScPrb) ), nl ) ),
		DTCRs = [D-T-C-Ru|Tru]
	),
	% ( Ru =:= 0 -> write( Ru is Yk / ScPrb ), nl ; true ),
	exact_daticoyk_to_daticoru( Tail, ScPrb, Tru ).

%%% end exact predicates

% paths_to_clausal_freqs( [], Tms, Count, Freqs ) :-
	% ( Count ==  0 ->
		% Freqs = [] 
		% ; 
		% kv_div_vs_by( Tms, Count, Freqs )
	% ).
% paths_to_clausal_freqs( [Path|Paths], AccTms, Count, Tms ) :-
	% flatten( Path, FlatPath ),
	% list_frequency( FlatPath, UnsFrq ),
	% keysort( UnsFrq, Frq ),
	% kvsi_merge_w_add( Frq, AccTms, NxAcc ),
	% NxCount is Count + 1,
	% paths_to_clausal_freqs( Paths, NxAcc, NxCount, Tms ).

terminate( CurrParams, Iter, ALl, BLl, NxtParams, Termin, Why ) :-
	bb_get( fam_write_parameters, WWhich/WWhere ),
	( WWhich == all ->
		write( WWhere, 'Iteration'(Iter) ), 
          write( WWhere, '.' ), nl( WWhere ),
		kv_write_long_list( NxtParams, 2, 2, WWhere, ',\t' )
		;
		true
	),
	( (memberchk(interactive,Termin),
	   terminate  ) ->
		Why = interactive_instruction
	;
	( (memberchk(iter(LimIter),Termin),
	   Iter >= LimIter ) ->
		Why = iteration_limit_reached
	;
	( (memberchk(prm_e(Epsilon),Termin),
	   e_limit_reached(CurrParams,NxtParams,Epsilon) ) ->
	   	Why = parameter_limit_reached(Epsilon)
	;
	( (memberchk(ll_e(LlE),Termin),
	   e_limit_reached([_-ALl],[_-BLl],LlE) ) ->
		Why = log_likelihood_limit_reached(LlE)
	;
	fail
	) ) ) ).

terminate :-
	write( 'terminate ? ' ),
	get_char( user_input, Inp ), 
	skip_a_line,
	( (Inp==y;Inp='Y') -> 
		true
	 	; 
		fail
	).

% e_limit_reached( [], [], _Epsilon ) :- fail.
e_limit_reached( [_N1-P1|T1], [_N2-P2|T2], Epsilon ) :-
	Mark is Epsilon - abs( P1 - P2 ) ,
	% write( (Mark is Epsilon - abs( P1 - P2 )) ), nl,
	( Mark > 0 -> 
		true
		;
		e_limit_reached( T1, T2, Epsilon )
	).

flatten_failure_path( Path, Acc, FlatPh ) :-
	( var(Path) -> 
		FlatPh = Acc
		;
		( Path = [H|T] -> 
			flatten_nv( H, FlatH ),
			append( Acc, FlatH, NxAcc ),
			flatten_failure_path( T, NxAcc,  FlatPh )
			;
			% it shouldnt really get here
			FlatPh = Acc
		)
	).

evaluate_parameters( [], [] ).
evaluate_parameters( [K-Expr|T], [K-Val|R] ) :-
	Val is Expr,
	evaluate_parameters( T, R ).

skip_a_line :- skip( 10 ).

write_ll(  Ll ) :-
	( bb_get( fam_write_ll, false ) ->
		true
		;
		( dbg_flag(on) -> 
			true 
			;
			write( log_likelihood(Ll) ), nl
		)
	).
