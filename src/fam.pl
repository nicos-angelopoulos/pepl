/** fam( Opts ).

     Run the failure adjusted maximisation (FAM) parameter estimation algorithm. 

For SLP source file jc_ml_S1.slp

==
0.5:: s(X,p) :- p(X), p(X).
0.5:: s(X,q) :- q(X).
0.5:: p(a).
0.5:: p(b).
0.5:: q(a).
0.5:: q(b).
==

and data file jc_ml_S1_data.pl
==
frequencies([s(a,p)-4,s(a,q)-3,s(b,p)-2,s(b,q)-3]).
==

the call succeeds with the learned PPs
     
==
?- fam( [goal(s(_A,_B)),slp(jc_ml_S1),datafile('jc_ml_S1_data.pl'),final_pps(PPs)] ).
PPs = [0.6602,0.3398,0.5858,0.4142,0.5,0.5]
==

Options 
  * count(CountMeth=exact)
    CountMeth in {*exact*, store, sample};

  * times(Tms=1000)
    only relevant with CountMeth=sample

  * termin(TermList)
    currently TermList knows about the following terms
    * interactive
      ask user if another iteration should be run
    * iter(I)
      I is the number of iterations
    * prm_e(E)
      parameter difference between iteration, that renders
      termination due to convergence of all parameters, between two iterations
   *  ll_e(L)
      likelihood convergence limit;

  * goal(Goal)
    the top goal, defaults to an all vars version of data;

  * pregoal(PreGoal)
    a goal that called only once, before experiments are run.
    The intuition is that PreGoal will partially instantiate Goal.

  * data(Data)
    the data to use, overrides datafile/1. Data should be
    a list of Yield-Times pairs. (All Yields of Goal should be included in
    Data, even if that means some get Times = 0.)

  * prior(Prior=none)
    the distribution to replace the probability labels with.
    By default prior is used, so input parameters are
    used as given in Slp source file. System also knows about uniform
    and random. Any other distribution should come in Prolog source
    file named Prior.pl and define Prior/3 predicate. First argument is a
    list of ranges (Beg-End) for each stochastic predicate in source file.
    Second argument, is the list of actual probability labels in source file.
    Finally, third argument should be instantiated to the list of labels
    according to Prior.

  * datafile(DataFile=data.pl)
    the data file to use, default is data.pl. DataFile
    should have either a number of atomic formulae or a single formula
    of the form : frequencies(Data).

  * complement(Complement=none)
    one of : none (with PrbSc = PrbTrue, the default), success (with PrbSc = 1 − PrbF ail),
    or quotient (with PrbSc = PrbT rue/(PrbT rue + PrbF ail)).

  * setrand(SetRand=false)
    sets random seeds. SetRand = true sets the seeds to some random triplet 
    while the default value _false_, does not set them. Any other value for
    SetRand is taken to be of the form rand(S1,S2,S3) as expected by system
    predicate random of the supported prolog systems.

  * eps(Eps=0)
    the depth Epsilon. Sets the probability limit under which
    Pepl considers a path as a failed one.

  * write_iterations(Wrt=all)
    indicates which set of parameters to output.  Values for Wrt are: all, last and none.

  * write_ll(Bool==true)
    takes a boolean argument, idicating where loglikelihoods should be printed or not.

  * debug(Dbg=off) 
    should be set to on or off. If on, various information about intermediate calculations will be printed.

  * return(RetOpts=[])
    a list of return options. The terms RetOpts contain variables. These will be instantiated
    to the appropriate values signified by the name of each corresponding term. 
    Recognised are, initial pps/1 for the initial parameters, final pps for the final/learned parameters,
    termin/1 for the terminating reason, ll/1 for the last loglikelyhood calculated, iter/1 for
    the number of iterations performed, and seeds/1 for the seeds used.

  * keep_pl(KeepBool==false)
    if true, the temporary Prolog file that contains the translated SLP, is not deleted.

  * exception(Handle=rerun)
    identifies the action to be taken if an exception is raised while running Fam.
    _rerun_ means that the same Fam call is executed repeatedly. Any other value for Handle will 
    cause execution to abort after printing the exception raised.

*/
fam( Opts ) :-
     option_sel( setrand, Opts, false, RndOpts, Rnd ),
     fam_setrand( Rnd, Seeds ),
     option_sel( return, RndOpts, [], _RetOpts, RetList ),
     ( memberchk(seeds(Seeds),RetList) -> true;true ),
     sload_in_fam_options( RndOpts, SldOpts ),
     sel_option_actions( slp(Slp), RndOpts, sload_pe(Slp,SldOpts), true ),
     sel_option_actions( datafile(Df), RndOpts, true, there_exists_mem_slp_with_datafile(Df) ),
     sel_option_actions( write_iterations(WchWhr), RndOpts, true, (WchWhr=all/user_output) ),
   ( WchWhr = Wch/Whr -> true; Wch = WchWhr, Whr = user_output ),
     bb_put( fam_write_parameters, Wch/Whr ),
     sel_option_actions( write_ll(Wll), RndOpts, true, Wll=true ),
     bb_put( fam_write_ll, Wll ),
     sel_option_actions( debug(Dbg), RndOpts, switch_dbg(Dbg), true ),
          % this is a peek:, the real one comes after Data is instantiated.
     ( memberchk( goal(Goal), RndOpts ) -> true; true),
     sel_option_actions( data(Data), RndOpts, true, 
          ( slp_data_file_location( Df, AbsDF ),
            datafile_to_frequencies( AbsDF, Goal, Data )
            ) ),
     check_data( Data ),
     FrqC = frequencies_to_top_goal( Data, Goal ),
     sel_option_actions( goal(Goal), RndOpts, true, FrqC ),
     sel_option_actions( pregoal(PrG), RndOpts, call(PrG), true ),
     sel_option_actions( prior(Prior), RndOpts, true, Prior=none ),
     set_prior( Prior ),
     sel_option_actions( termin(_Termin), RndOpts, PadOpts = RndOpts, PadOpts = [termin([interactive])|Opts] ),
     sel_option_actions( count(_Cnt), PadOpts, EsmOpts=PadOpts, EsmOpts = [count(exact)|PadOpts] ),
     % setrand/1 is also recognised (in estim/4). 
     % either false, true or rand(R1,R2,R3).
     % sel_option_actions( setrand(_Cnt), EsmOpts, EsmOpts=FinOpts, EsmOpts = [(exact)|PadOpts] ),
     % estim( Goal, Data, EsmOpts, _Params ).
     sel_option_actions( exception(Handle), RndOpts, true, Handle=abort ),
     % sel_option_actions( exception(Handle), RndOpts, true, Handle=rerun ),
     estim( Goal, Data, EsmOpts, _Params ).
     % Estim = estim( Goal, Data, EsmOpts, _Params ),
     % catch( Estim, All, fam_excp( All, Handle, Estim, Slp, Prior ) ).

/*
fam_excp( Error, Handle, Estim, Slp, Prior ) :-
     write( user_error, 'The following exception was caught while trying to run FAM. Atttempting to rerun fam/1 with same arguments.' ), nl( user_error ),
     print_message( error, Error ),
     fam_excp1( Handle, Estim, Slp, Prior ).

fam_excp1( rerun, Estim, Slp, Prior ) :-
     sload_pe( Slp ),
     set_prior( Prior ),
     catch( Estim, All, fam_excp(All,rerun,Estim,Slp,Prior) ).
fam_excp1( _Handle, _Estim, _Slp, _Prior ) :-
     abort.
*/
