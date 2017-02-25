:- module( pepl, [
			    fam/1,
			    sload_pe/1,
			    sload_pe/2,
			    ssave/1,
                   switch_dbg/1,  % should we be using debug/1 ?
			    dbg_pepl/1,
			    pepl_citation/2,
			    pepl_version/2,
			    sls/0,
			    scall/1,
			    scall/6,
			    % all_path/2,
			    op( 600, xfy, :: )
			  ] ).

% employs requires...

% :- ensure_loaded( '../src/all_path' ).
:- ensure_loaded( '../src/init_lib' ).
:- ensure_loaded( '../src/estim'     ).         % /4.
:- ensure_loaded( '../src/myoption'  ).			
:- ensure_loaded( '../src/sload_pe'  ).			
:- ensure_loaded( '../src/set_prior' ).			
:- ensure_loaded( '../src/slp_file_location' ).		% /2.

:- ensure_loaded( library(datafile_to_frequencies) ).	% /4.
:- ensure_loaded( library(lists) ).				% append/3.
:- ensure_loaded( library(mold_vars_list) ).			% /2.
:- ensure_loaded( library(fam_setrand) ).			% /2.
:- ensure_loaded( library(werr) ).			          % werrc/3.

:- dynamic( dbg_flag/1 ).

:- license( mit ).

/** <module> An implementation of the FAM algorithm.

Pepl is an implemention of the failure adjusted (FAM) algorithm which does 
parameter estimation (PE) of the probability labels of stochastic logic programs (SLPs).

See documentation fam/1 for details on how to run parameter estimation on SLPs.

Example stochastic programs are in directory =|slp|= and example run scripts are in =|examples|=.

Licence
---
This software is distributed under the MIT licence.

## Installation and testing ...

Pepl runs on current versions of SWI (7) and Yap (6.3).

### ... on SWI

==
pack_install(pepl).
[library(pepl)].
[pack('pepl/examples/main')].
main.
==

### ... on Yap

Download latest sources from http://stoics.org.uk/~nicos/sware/pepl <br>
or https://github.com/nicos-angelopoulos/pepl

== 
gunzip pepl-*tgz
tar xf pepl-*tar
cd pepl-*
cd examples
yap
[main].
main.
==

## Package information

@author Nicos Angelopoulos
@license This software is distributed under the MIT licence
@version 2.1, 2017/2/25
@version 2.0.6, 2014/01/28 
@see  the user guide at pack('pepl/doc/pepl-user_guide.pdf').
@see James Cussens. Parameter estimation in stochastic logic programs. Machine Learning, 44(3):245-271, 2001. ftp://ftp.cs.york.ac.uk/pub/aig/Papers/james.cussens/jcslpmlj.pdf
@see Nicos Angelopoulos, Notes on the implementation of FAM, 3rd Probabilistic Logic Programming workshop (a ILP 2016 workshop), 03/09/2016, http://ceur-ws.org/Vol-1661/paper-05.pdf
@see pepl website http://stoics.org.uk/~nicos/sware/pepl
*/

% :- switch_dbg( off ).
% :- switch_dbg( on ).


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

the call 
	
==
   fam( [goal(s(_A,_B)),slp(jc_ml_S1),datafile('jc_ml_S1_data.pl'),final_pps(PPs)] ).
==

   succeeds with 
==
   PPs = [0.6602,0.3398,0.5858,0.4142,0.5,0.5]
==


Options: 
	* count(CountMeth), CountMeth in {*exact*, store, sample};

	* times(Tms), default is Tms = 1000 (only relevant with CountMeth=sample);

	* termin(TermList), currently TermList knows about the following terms
		* interactive- ask user if another iteration should be run,
          * iter(I)- I is the number of iterations,
          * prm e(ǫ p )- parameter difference between iteration, that renders
		   termination due to convergence of all parameters, between two iterations,
          *  ll e(ǫ λ )- likelihood convergence limit;
	
	* goal(Goal), the top goal, defaults to an all vars version of data;
	* pregoal(PreGoal), a goal that called only once, before experiments
		are run. The intuition is that PreGoal will partially instantiate Goal.
	* data(Data), the data to use, overrides datafile/1. Data should be
		a list of Yield-Times pairs. (All Yields of Goal should be included in
		Data, even if that means some get Times = 0.)
	
	* prior(Prior), the distribution to replace the probability labels with.
		Default is that no prior is used, Prior=none, input parameters are
		used as given in Slp source file. System also knows about uniform
		and random. Any other distribution should come in Prolog source
		file named Prior.pl and define Prior/3 predicate. First argument is a
		list of ranges (Beg-End) for each stochastic predicate in source file.
		Second argument, is the list of actual probability labels in source file.
	     Finally, third argument should be instantiated to the list of labels
		according to Prior.

	* datafile(DataFile), the data file to use, default is SLP data.pl. DataFile
		should have either a number of atomic formulae or a single formula
			of the form : frequencies(Data).

	* complement(Complement), one of : none (with PrbSc = PrbT rue,
		the default), success (with PrbSc = 1 − PrbF ail), or quotient (
		with PrbSc = PrbT rue/(PrbT rue + PrbF ail)).

	* setrand(SetRand), sets random seeds. SetRand = true sets the seeds
		to some random triplet while the default SetRand = false, does
		not set them. Any other value for SetRand is taken to be of the
		form rand(S1,S2,S3) as expected by system predicate random of
		the supported prolog systems.

	* eps(Eps), the depth Epsilon. Sets the probability limit under which
		Pepl considers a path as a failed one.

	* write_iterations(Wrt) indicates which set of parameters to output.
		Values for Wrt are: all, which is the default, last, and none.

	* write_ll(Bool) takes a boolean argument, idicating where loglikelihoods should be printed or not. Default is true.

	* debug(Dbg) should be set to on or off (later is the default). If on,
		various information about intermediate calculations will be printed.

	* return(RetOpts), a list of return options, default is the empty list.
		The terms RetOpts contain variables. These will be instantiated
		to the appropriate values signified by the name of each corresponding term. 
		Recognised are, initial pps/1 for the initial parameters, final pps for the final/learned parameters,
		termin/1 for the terminating reason, ll/1 for the last loglikelyhood calculated, iter/1 for
		the number of iterations performed, and seeds/1 for the seeds used.

	* keep_pl(KeepBool), if true, the temporary Prolog file that contains
		the translated SLP, is not deleted. Default is false.

	* exception(Handle), identifies the action to be taken if an exception
		is raised while running Fam. The default value for Handle is rerun.
		This means the same Fam call is executed repeatedly. Any other value for Handle will 
		cause execution to abort after printing the ex- ception raised.

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

%% pepl_citation( -Atom, -Bibterm ).
%
% This predicate succeeds once for each publication related to this library.
% Atom is the atom representation
% suitable for printing while Bibterm is a bibtex(Type,Key,Pairs) term of the same publication.
% Produces all related publications on backtracking.
%
pepl_citation( Atom, bibtex(Type,Key,Pairs) ) :-
	Atom = 'Notes on the implementation of FAM\nNicos Angelopoulos\n3rd Probabilistic Logic Programming workshop (PLP 2016, a workshop of ILP 2016). September 2016, Imperial College London. Pages 46-58.',
    Type = inproceedings,
    Key  = 'AngelopoulosN+2016',
    Pairs = [
               author = 'Nicos Angelopoulos',
               title  = 'Notes on the implementation of FAM',
               booktitle = '3rd Probabilistic Logic Programming Workshop (collocated with ILP 2016)',
               year = 2016,
               month = 'September',
               address = 'Imperial College, London',
			publisher = 'CEUR',
			volume   = '1661',
               url     = 'http://ceur-ws.org/Vol-1661/'
     ].



%% pepl_version( -Version, Date ).
%
%  Pepl's current Version (Maj:Min:Fix) and publication date (date(Year,Month,Day)).
%
% pepl_version( 2:0:6, date(2014,1,28) ).
pepl_version( 2:1, date(2017,2,25) ).

there_exists_mem_slp_with_datafile( DataFile ) :-
	( bb_get( current_slp, Cslp ) -> 
		true
		;
          pepl_msg( 'No SLP currently in memory' )
	),
	( file_name_extension(Stem,slp,Cslp) -> true; Stem=Cslp ),
	% fname_stem( Cslp, ".slp", Stem, _FullExt ),
	atom_concat( Stem, '_data', DataFile ).
	% atom_codes( Stem, StemCs ),
	% append( StemCs, "_data", DataFileCs ),
	% atom_codes( DataFile, DataFileCs ).
	
frequencies_to_top_goal( [H-_Hocc|T], Goal ) :-
	H =.. [Name|Args], 
	mold_vars_list( Args, Vars ),
	Goal =.. [Name|Vars],
	frequencies_to_top_goal_1( T, Goal ).

frequencies_to_top_goal_1( [], _Goal ).
frequencies_to_top_goal_1( [H-_Hocc|T], Goal ) :-
	( \+ \+ Goal = H -> 
		true
		;
          pepl_msg( ['Skipping incompatible datum ',Goal,' and ', H] )
	),
	frequencies_to_top_goal_1( T, Goal ).

check_data( Data ) :-
     ( is_list(Data) ->
          ( Data == [] ->
               Inner = empty_list
               ;
               ( check_data_1(Data) ->
                    Inner = []
                    ;
                    Inner = list_is_not_pairs(Data)
               )
          )
          ;
          Inner = not_a_list(Data)
     ),
     ( Inner == [] ->
          true
          ;
          pepl_msg( data_format_error(Inner) )
     ).

check_data_1( [] ).
check_data_1( [_G-_F|T] ) :-
	!,
	check_data_1( T ).

sload_in_fam_options( [], [] ).
sload_in_fam_options( [H|T], SldOpts ) :-
	( \+ memberchk( H, [keep_pl(_)] ) ->
		SldOpts = TSldOpts
		;
		SldOpts = [H|TSldOpts]
	),
	sload_in_fam_options( T, TSldOpts ).

pepl_msg( Msg ) :-
     werrc( Msg, err, abort ).

pepl_msg( Msg, Pfx ) :-
     werrc( Msg, Pfx, abort ).

pepl_msg( PrvMsg, PrvPfx, Call ) :-
     ( is_list(PrvMsg) -> Msg = PrvMsg ; Msg = [PrvMsg] ),
     ( PrvPfx == warn ->
          Pfx = 'Pepl Warning: '
          ;
          ( PrvPfx == err -> Pfx = 'Pepl Error: '
               ;
               PrvPfx = Pfx
          )
     ),
     werrc( Msg, Pfx, Call ).



%% ssave( +File ).
%
% Save the stochastic program currently in memory to a file.
%
ssave( InFile ) :-
	( InFile == user -> 
		bb_get( directives, Directvs ),
		portray_sdirectives( Directvs ), nl,
		bb_get( all_slp_clauses, AllClauses ),
		bb_get( pp, PPs ),
		portray_sclauses( AllClauses, PPs )
		;
		( file_name_extension(_Base,slp,InFile) ->
			File = InFile
			;
			file_name_extension( InFile, slp, File )
		),
		current_output( Co ),
		open( File, write, Stream ),
		( ( set_output( Stream ),
			% DefMod = (write( (:- module( slp, [])) ), nl, nl),
			% pl( swi(_), DefMod ), 
			write( '% Generated by ssave/1.' ), nl, nl,
			bb_get( directives, Directvs ),
			portray_sdirectives( Directvs ), nl,
			bb_get( all_slp_clauses, AllClauses ),
			bb_get( pp, PPs ),
			portray_sclauses( AllClauses, PPs ),
			set_output( Co ),
			write( 'program saved in: ' ), write( File ), nl, !
			)
			;
			set_output( Co ),
			write( 'failure while trying to save in: ' ), write( File ), nl
		),
		close( Stream ),
		set_output( Co )
	).

%% sls.
%
%  Listiing of the stochastic program currently in memory.
%
sls :-
	% bb_get( all_transformed_clauses, TrsClauses ),
	% sprint_list( TrsClauses ).
	ssave( user ).

%% sload_pe( Files ).
%% sload_pe( Files,  Options ).
%
% Load an SLP to memory. If the source file has an slp extension the extension
% may be omitted. Pepl looks in the following directories and order for the source
% file(s). =|.|=, and =|./slp/|= while on SWI it also looks in,
% =|pack(’pepl/slp/’)|=.
%
sload_pe( File ) :-
	sload_pe( File, [] ).

sload_pe( Files, InOptions ) :-
	% bb_get( cc,  ),
	bb_put( pp, [] ),
	bb_put( all_slp_clauses, [] ),
	bb_put( all_transformed_clauses, [] ),
	bb_put( directives, [] ),
	sload_options_defaults( DefOpts ),
	options_cohesion( InOptions, DefOpts, Options ),
	once( select( add_mode(AddMode), Options, Opts1 ) ),
	( AddMode == append ->
		true
		;
		( AddMode == write ->
			clean_slp_module
			;
			pepl_msg( improper_add_mode_option(AddMode,in(sload_pe/2)) )
		)
	),
	sload_pe_1( Files, Opts1, 1, CC ),
	bb_put( cc, CC ),
	bb_put( current_slp, Files ).

/** scall( Goal ).

Sample Goal. 

== 
?- sload_pe( [pack(pepl/slp/coin] ).
==
*/
scall( Goal ) :-
	Eps is 1E-10,
	scall_1( sample, Goal, Eps, _Path, Succ, _Prb ), 
	Succ \== fail. % fixme: or false ?

scall( Goal, Path, Succ, Prb ) :-
	scall_1( all, Goal, 0, Path, Succ, Prb ). 

/** scall( Goal, Eps, Meth, Path, Succ, Prb ).

This predicate  is for people interested in the iternals of pepl.
Use at your own peril.

The predicate arguments are as follows.
	* The vanilla prolog Goal to call. 
	* The value of Eps(ilon) at which branches are to be considered as failures.
	* The search Method to be used, (all for all solutions or sample for a single solution).
	* The Path(s) of the derivation(s). 
	* A flag idicating a Succ(essful) derivation or otherwise-Succ is bound to the atom fail
	  if this was a failed derivation and remains unbound otherwise.
	  BrPrb the branch probability of the derivation.
	  if this was a failed derivation and remains unbound otherwise.
	* BrPrb the branch probability of the derivation.

See predicate main_gen/1, in examples/main_scfg.pl for example usage.

*/
scall( Goal, Eps, Meth, Path, Succ, Prb ) :-
	scall_1( Meth, Goal, Eps, Path, Succ, Prb ).

dbg_flag( off ).

:- ensure_loaded( library(write_list_with_line_numbers) ).

%% switch_dbg( Switch ). 
% 
%  Switch debugging of fam/1 to either =|on|= or =|off|=.
%
switch_dbg( Flag ) :-
	( (Flag == on;Flag == off) -> 
		retractall( dbg_flag(_) ),
		assert( (dbg_flag(Flag):- !) )
		;
		G = switch_dbg( Flag ), T=[off,on],
		print_message( error, type_error(G,1,T,Flag) )
	).

%% dbg_pepl( +Goal ).
%
% Call Goal iff in (pepl) debugging. 
%
dbg_pepl( Goal ) :- 
	( dbg_flag(on) -> 
		call( Goal )
 		; 
		true
	).

dbg_ls_pepl( Header, List ) :-
	( dbg_flag(on) -> 
		write( Header ), nl,
		write_list_with_line_numbers( List, 1, 4 ), nl
		;
		true
	).
