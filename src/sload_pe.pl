% :- op( 1200, xfy, :: ).

% this has(/had) following changes from ../pe/src/sload.pl
%  sample method complement.
%  normalisation is optional and controllable with options
%   of sload_pe/2.

:- ensure_loaded(init_lib).                     % library/1.
:- ensure_loaded(library(slp_file_location)).   % /2.
:- ensure_loaded(library(read_terms_cons)).     % /5.
:- ensure_loaded(library(portray_clauses_on)).  % /2.
:- ensure_loaded(library(flatten_nv)).          % flatten_nv/2.
:- ensure_loaded(library(kvsi_till_left_incl)). % /3.
:- ensure_loaded(library(kvsi_val_w_right_rem)).% /3.
:- ensure_loaded(library(mold_vars_list)).      % /2.
:- ensure_loaded(library(expand_sgoal)).        % /2, /10.
:- ensure_loaded(library(options_cohesion)).    % /3.
:- ensure_loaded(library(is_list_of_n_vars)).   % /2.

portray_sdirectives( [] ).
portray_sdirectives( [H|T] ) :-
     write( ':- ' ), write( H ), write( '.' ), nl,
     portray_sdirectives( T ).

% portray( if(A,B,C) ) :-
     % write( 'if( ' ), write(A), write(','), nl,
     % tab(2), write(B), write(','), nl,
     % tab(1), write(C), write(')'), nl.

portray_sclauses( [], _Empty ).
portray_sclauses( [H-HVns|T], PPs ) :-
     ( H = (_OldLbl::Clause) -> 
          PPs = [_Id-Hp|TPs],
          write( Hp ), write( ' :: ' ),
          % portray_clause( Clause )
          sprint( Clause, HVns )
          ;
          % portray_clause( H ),
          TPs = PPs,
          sprint( H, HVns )
     ),
     portray_sclauses( T, TPs ).
     
slisting :- 
     pl( swi(_), pepl_slp:listing, 
          (module(pepl_slp),listing,module(user))
     ).   

sprint_list( [] ).
sprint_list( [H|T] ) :-
     sprint( H, [] ),
     sprint_list( T ).

slp_clause_succeeded( Succ ) :-
     Succ \== fail.

sload_options_defaults( [
          % normalise(true),
          normalise(false),
          compilation_mode(consult),
          add_mode(write),
          keep_pl(false)
          ] ).

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
               pepl_warn( add_mode_opt(AddMode) )
          )
     ),
     sload_pe_1( Files, Opts1, 1, CC ),
     bb_put( cc, CC ),
     bb_put( current_slp, Files ).

sload_pe_1( Files, Options, CC, Fcc ) :-
     ( Files = [File|Tail] -> 
          sload_pe_1( File, Options, CC, CC1 ), 
          sload_pe_1( Tail, Options, CC1, Fcc )
          ;
          ( Files = []  -> 
               Fcc is CC
               ;
               File = Files,
               slp_file_location( File, FileName ),
               print_message( informational, pepl(sload_src(FileName)) ),
               RdOpts = [variable_names(VNms)],
               read_terms_cons( FileName, -, RdOpts, VNms, AllClausesPrv ),
               maplist( clause_parenthesis, AllClausesPrv, AllClauses ),

               once( select( normalise(Norm), Options, _ROs ) ),
               assert_initial_pp( AllClauses, Norm, Scls, CC, TrsClauses, Fcc),
               bb_get( all_slp_clauses, CurrSlpCls ),
               bb_get( all_transformed_clauses, CurrTrsCls ),
               append( CurrSlpCls, Scls, NewSlpCls ),
               append( CurrTrsCls, TrsClauses, NewTrsCls ),
               bb_put( all_slp_clauses, NewSlpCls ),
               bb_put( all_transformed_clauses, NewTrsCls ),
               file_name_extension( FileName, tmp, TmpFile ),
               portray_clauses_on( TmpFile, TrsClauses ), 
               print_message( informational, pepl(sload_file(TmpFile)) ),
               load_tmp_file( TmpFile ),
               ( memberchk( keep_pl(true), Options ) ->
                    true
                    ;
                    delete_file( TmpFile )
               )
          )
     ).

clean_slp_module :-
     current_predicate(pepl_slp:Name/Arity),
     Arity > 6,
     abolish(pepl_slp:Name/Arity),
     fail.
clean_slp_module.

load_tmp_file( TmpFile ) :-
     pl( yap(_Ay),
          (source,
          % module(pepl_slp),
          % use_module(TmpFile),
          pepl_slp:consult(TmpFile),
          %% slp:consult(TmpFile),
          no_source)
       ),
     % pepl_slp:consult(TmpFile),
     pl( sicstus(_As), pepl_slp:consult(TmpFile) ),
     pl( swi(_Ap), pepl_slp:consult(TmpFile) ),
     module( user ).

assert_initial_pp( Clauses, NmO, Scls, CCin, TrsClauses, CCout ) :-
     % write( initial_pp_in(CCin,CCout) ), nl,
     initial_pp( Clauses, NmO, NestScls, CCin, NTClauses, NestPPs, CCout ),
     flatten_nv( NestScls, Scls ),
     % write( initial_pp_out(CCin,CCout) ), nl,
     % retractall( pp(_) ),
     flatten_nv( NestPPs, ThesePPs ),
     flatten_nv( NTClauses, TrsClauses ),
     bb_get( pp, CurrPPs ),
     append( CurrPPs, ThesePPs, PPs ),
     bb_put( pp, PPs ).

initial_pp( [], _NmO, [], Nth, [], [], Nth ).
initial_pp( [H-HVns|T], NmO, Scls, Nth, PlCls, PPs, Kth ) :-
     /*
     ( HPrv = ( PrvLbl :: PrvHead :- PrvBody ) -> 
               H = (PrvLbl :: (PrvHead :- PrvBody))
               ;
               HPrv = H
     ),
     */
     Oth is Nth + 1,
     ( H = (:- Directive ) -> 
          % call( Directive ), 
          call( pepl_slp:Directive ),
          bb_get( directives, Directvs ),
          bb_put( directives, [Directive|Directvs] ),
          % write( call(Directive) ), nl,
          initial_pp( T, NmO, Scls, Nth, PlCls, PPs, Kth )
          ;
          Scls = [[H-HVns|HScls]|TScls],
          ( H=(PrbAtom::Clause) -> 
               Type = s,
               ( atom(PrbAtom ) ->
                    atom_codes( PrbAtom, PrbCs ),
                    number_codes( Prb, PrbCs )
                    ;
                    Prb = PrbAtom
               ),
               ( NmO == complement ->
                    exp_s_clause( Clause, _None1, Oth, Spec, Ead, OthClause ),
                    Qth is Oth + 1,
                    collect_spp( T, Ead, HScls, Prb, Qth, Spps, HColl, Rest, Sum, Nxt ),
                    Complement is 1 - Sum,
                    ( Complement > 1.0e-05 -> 
                         Hpp = [Nth-Complement,Oth-Prb|Spps],
                         % write( (Hpp = [Nth-Complement,Oth-Prb|Spps]) ), nl,
                         Spec = Name/Arity,
                         is_list_of_n_vars( Arity, VacArgs ),
                         % exp_s_clause( VarHead, _None2, Nth, Spec, Ead, NthClause ),
                         NthClause =.. [Name,Nth,SomePin,_Eps,_Sel,[Nth],fail,SomePin|VacArgs],
                         NthBeg = Nth,
                         PlCls = [NthClause,OthClause,HColl|Tplcs]
                         ;
                         Hpp = [Oth-Prb|Spps],
                         NthBeg = Oth,
                         PlCls = [OthClause,HColl|Tplcs]
                    )
                    ;
                    NthBeg = Nth,
                    exp_s_clause( Clause, _None3, Nth, Spec, Ead, OthClause ),
                    % portray_clause( OthClause ),
                    collect_spp( T, Ead, HScls, Prb, Oth, Spps, HColl, Rest, Sum, Nxt ),
                    ( NmO == true ->
                         normalise_pps( [Nth-Prb|Spps], Sum, Hpp )
                         ;
                         Hpp = [Nth-Prb|Spps]
                    ),
                    PlCls = [OthClause,HColl|Tplcs]
               ),
               PPs = [Hpp|Tpps]
               ;
               PPs = Tpps,
               NthBeg = Nth,
               % the following has already been tested for,
               % remove .... 
               ( H = (:- Directive) -> 
                    call( pepl_slp:Directive ),
                    bb_get( directives, Directvs ),
                    bb_put( directives, [Directive|Directvs] ),
                    PlCls = Tplcs,
                    PPs = Tpps,
                    Nxt is Nth,
                    Rest = T
                    ;
                    Type = ns,
                    exp_ns_clause( H, _NoneNS, Nth, Spec, Ead, NthClause ),
                    write( ns(H) ), nl,
                    write( tns(NthClause) ), nl,
                    collect_nspp( T, Ead, HScls, Oth, HColl, Rest, Nxt  ),
                    PlCls = [[NthClause|HColl]|Tplcs]
               )
          ),
          retractall( spec_cids(Spec, _OldType, _OldOth, _OldNxt) ),
          Prev is Nxt - 1,
          % write( assert( spec_cids( Spec, Type, NthBeg, Prev ) ) ), nl,
          assert( spec_cids( Spec, Type, NthBeg, Prev ) ),
          initial_pp( Rest, NmO, TScls, Nxt, Tplcs, Tpps, Kth )
     ).

exp_ns_clause( InClause, Match, Nth, Spec, Ead, NthClause ) :-
     expand_term( InClause, Clause ),
     ( Clause = (Head:-Body) -> 
          \+ \+ Head = Match,
          alpha( Head, Functor, Alpha, Args ),
          Ead =.. [Functor|Alpha],
          Nead =.. [Functor,Nth,Pin,Eps,Sel,Paths,Succ,Pout|Args],
          body_expand( Body, Pin, Eps, Sel, Paths, Succ, Pout, true, ExpBody ),
          NthClause = (Nead:-ExpBody)
          ;
          \+ \+ Clause = Match,
          alpha( Clause, Functor, Alpha, Args ),
          Ead =.. [Functor|Alpha],
          NthClause =.. [Functor,Nth,Pin,_Eps,_Sel,[],_Succ,Pin|Args]
     ),
     length( Args, Arity ),
     Spec = Functor/Arity.

exp_s_clause( InClause, Match, Nth, Spec, Ead, NthClause ) :-
     expand_term( InClause, Clause ),
     ( Clause = (Head:-Body) -> 
          \+ \+ Head = Match,
          alpha( Head, Functor, Alpha, Args ),
          Ead =.. [Functor|Alpha],
          Nead =.. [Functor,Nth,Pin,Eps,Sel,[Nth|Paths],Succ,Pout|Alpha],
          Cond = (Alpha=Args),
          body_expand( Body, Pin, Eps, Sel, Paths, Succ, Pout, Cond, ExpBody ),
          NthClause = (Nead:-ExpBody)
          ;
          \+ \+ Clause = Match,
          alpha( Clause, Functor, Alpha, Args ),
          Ead =.. [Functor|Alpha],
          NthHead =.. [Functor,Nth,Pin,_Eps,_Sel,Path,Succ,Pin|Alpha],
          NthClause = (NthHead:- 
               (Alpha=Args-> Path=[Nth];Path=[Nth],Succ=fail) )
               
     ),
     length( Args, Arity ),
     Spec = Functor/Arity.

% collect_spp( Pairs, Term, StochClauses, SumAcc, ZthAcc, IdPrbPairs, ExpClauses, RemPairs, Sum, Zth ).
% Pair = ReadInClause-ClauseVars
% Term = selector for all first n Pairs that have matching ClauseHead
% StochClause = matched Pair, 
% IdPrbPair = as used in PPs. (bb_get( pp,PPs) ). 
% ExpClause = expanded matched Clause.
% RemPair = not matching pairs.
% Sum = sum of matched probability pairs.
% Zth = one bigger than the last Id used.
collect_spp( [], _Term, [], Sum, Zth, [], [], [], Sum, Zth ).
collect_spp( [H-HVns|T], Term, Scls, AccPrb, Nth, Spps, Coll, Rest, Sum, Zth ) :-
     ((H = (PrbAtom::Clause), 
           exp_s_clause(Clause,Term,Nth,_Spec,_Ead,NthClause)) -> 
                    % portray_clause( NthClause ),
           Scls = [H-HVns|TScls],
          ( atom(PrbAtom ) ->
               atom_codes( PrbAtom, PrbCs ),
               number_codes( Prb, PrbCs )
               ;
               Prb = PrbAtom
          ),
          NxAcc is AccPrb + Prb,
          NxNth is Nth + 1,
          Coll =  [NthClause|Cest],
          Spps = [Nth-Prb|Tpps],
          collect_spp( T, Term, TScls, NxAcc, NxNth, Tpps, Cest, Rest, Sum, Zth )
          ;
          ( (\+ memberchk((Term:-_Body),[H|T]), \+  memberchk(Term,[H|T]) ) -> 
               Spps=[], Coll=[], Rest=[H-HVns|T], Sum=AccPrb, Zth=Nth, Scls = []
               ;
               pepl_warn( mixed_clause )
          )
     ).
     
collect_nspp( [], _Term, [], Nth, [], [], Nth ).
collect_nspp( [H-HNvs|T], Term, Scls, Nth, Coll, Rest, Zth ) :-
     ( exp_ns_clause( H, Term, Nth, _Spec, _Ead, NthClause ) ->
          NxNth is Nth + 1,
          Coll = [NthClause|Cest],
          Scls = [H-HNvs|TScls],
          collect_nspp( T, Term, TScls, NxNth, Cest, Rest, Zth )
          ;
          ( (\+ memberchk((_Prb::(Term:-_Body)),[H|T]), \+  memberchk(Term,[H|T])) ->
          % ( (\+ memberchk((_Prb::Term:-_Body),[H|T]), \+  memberchk(Term,[H|T])) ->
               Rest = [H-HNvs|T], Zth = Nth, Coll = [], Scls = []
               ;
               pepl_warn( dixed_clause(H,Term) )
          )
     ).
     
% normalise_pps( PPs, Sum, Npps ) :-
     % ( Sum =:= 1  -> % Npps = PPs
          % ; % normalise_pps_1( PPs, Sum, Npps ) % ).
% this was normalise_pps_1/3.
normalise_pps( [], _Sum, [] ).
normalise_pps( [Id-Prb|T], Sum, [Id-NewPrb|NT] ) :-
     NewPrb is Prb / Sum,
     normalise_pps( T, Sum, NT ).

alpha( Term, Functor, Alpha, Args ) :-
     Term =.. [Functor|Args],
     mold_vars_list( Args, Alpha ).

all_current_to_uniform :-
     bb_get( pp, PPs ),
     findall( Beg-End, spec_cids( _Spec, s, Beg, End ), Ranges ),
     all_ranges_to_uniform( Ranges, PPs, NewPPs ),
     bb_put( pp, NewPPs ).

all_ranges_to_uniform( [], PPs, PPs ).
all_ranges_to_uniform( [B-E|T], AccPPs, PPs ) :-
     Prb is E - B + 1,
     range_updates_pps( B, E, Prb, AccPPs, NxPPs ),
     all_ranges_to_uniform( T, NxPPs, PPs ).

range_updates_pps( R, E, Prb, PPs, NwPPs ) :-
     kv_update( PPs, R, Prb, NxPPs ),
     ( R == E ->
          NwPPs = NxPPs
          ;
          range_updates_pps( R, E, Prb, NxPPs, NwPPs )
     ).

% has_spec_cids( Spec, Type, Beg, End ) :- 
     % ( spec_cids(Spec,Type,Beg,End) ->
          % true
          % ;
          % current_predicate( Spec ), Type = ns,
          % Beg = nan, End = nan
     % ).

% Select e {sample,all,top_sample}
rc( Sel, Spec, ClId, Prb ) :-
     % ( has_spec_cids( Spec, Type, Beg, End ) ->
     ( spec_cids( Spec, Type, Beg, End ) ->
          ( Type == s -> 
               ( (Sel = sample;Sel == top_sample) ->
                    bb_get( pp, PPs ), 
                    kvsi_val_w_right_rem( PPs, Beg, Fval, Right ), 
                    random( Rnd ),
                    ( Rnd < Fval ->
                         ClId = Beg, Prb = Fval
                         ;
                         RdRnd is Rnd - Fval,
                         key_strip_on_pval( Right, RdRnd, Fval, End, ClId, Prb )
                    )
                    ; % assume Sel == all
                    bb_get( pp, PPs ), 
                    kvsi_val_w_right_rem( PPs, Beg, Fval, Right ),
                    ( Beg =:= End -> 
                         Left = []
                         ;
                         kvsi_till_left_incl( Right, End, Left )
                    ),
                    member( ClId-Prb, [Beg-Fval|Left] )
                    % write( clid_prb( ClId-Prb ) ), nl
               )
               ; % Type = ns
               Prb = 1,
               ( Sel = sample ->
                    true
                    ; % does the following make sense ?
                    ( Sel == top_sample ->
                         % ( number(Upper) ->
                              Upper is End + 1,
                              random( Beg, Upper, ClId )
                              % ; % otherwise we calling an non-meta pred
                              % ClId = 0
                         % )
                         ; % assume Sel == all
                         true
                    )
               )
          )
          ;
          pepl_error( unknown_predicate(Spec) )
     ).
     % write( rc( Sel, Spec, ClId, Prb ) ), nl.
     
max_val( [], MaxVal, MaxKey, MaxVal, MaxKey ).
max_val( [Hk-Hv|T], CurMaxV, CurMaxK, MaxVal, MaxKey ) :-
     ( Hv > CurMaxV -> 
          NxMaxV is Hv, NxMaxK = Hk
          ;
          NxMaxV is CurMaxV, NxMaxK = CurMaxK
     ),
     max_val( T, NxMaxV, NxMaxK, MaxVal, MaxKey ).

key_strip_on_pval( [], _Rnd, _Fval, _End, _ClId, _Prb ) :-  
     pepl_error( off_the_cliff ).
key_strip_on_pval( [Hk-Hv|T], Rnd, Fval, End, ClId, Prb ) :-
     ( Rnd < Hv  -> 
          ClId = Hk, Prb is Hv
          ;
          ( Hk=End -> 
               pepl_error( off_the_rocking_horse(Hk,End) )
               ;
               RdRnd is Rnd - Hv,
               key_strip_on_pval( T, RdRnd, Fval, End, ClId, Prb )
          )
     ).

ns_call_1( A, Eps, Sel, Pin, AllPaths, Succ, Pout ) :-
     ( ( predicate_property(A,built_in),LoF=user;
          predicate_property(pepl_slp:A,imported_from(LoF)) ) -> 
          AllPaths = [0], 
          Pout is Pin,
          % if( LoF:A, true, Succ = fail )
          LoF:A
          ;
          % alpha( A, Functor, Alpha, Args ),
          A =.. [Functor|Args],
          Nead =.. [Functor,_Nth,Pin,Eps,Sel,AllPaths,Succ,Pout|Args],
          call( Nead )
     ).


body_expand( Body, Pin, Eps, Sel, Paths, Succ, Pout, Cond, ExpBody ) :-
     ( Cond == true -> 
          ExpBody = NestBody
          ;
          ExpBody = % if(
               % (Cond,Eps<Pin,NestBody),
               % true,
               % (Pout is Pin,Paths=[],Succ=fail)
               % )
               ( Eps > Pin ->
                    (Paths=[],Succ=fail,Pout is Pin)
                    ;
                    ( Cond -> 
                         NestBody
                         ;
                         (Paths=[],Succ=fail,Pout is Pin)
                    )
               )

     ),
     body_expand( Body, Pin, Eps, Sel, Paths, Succ, Pout, NestBody ).

body_expand( (A->B;C), Pin, Eps, Sel, AllPaths, Succ, Pout, ExpITE ) :-
     !,
     ExpITE = (AllPaths = [IfPh|TEPh],(ExpA,(\+NwSucc==fail)->ExpB;ExpC)),
     body_expand( A, Pin, Eps, Sel, IfPh, NwSucc, Pnxt, ExpA ),
     body_expand( B, Pnxt, Eps, Sel, TEPh, Succ, Pout, ExpB ),
     body_expand( C, Pin, Eps, Sel, TEPh, Succ, Pout, ExpC ).

body_expand( sif(If,Then,Else), Pin, Eps, Sel, AllPaths, Succ, Pout, ExpITE ) :-
     !,
     ExpITE = (AllPaths = [IfPh|TEPh],ExpIf,(\+NwSucc==fail->ExpThen;ExpElse)),
     body_expand( If, Pin, Eps, Sel, IfPh, NwSucc, Pnxt, ExpIf),
     body_expand( Then, Pnxt, Eps, Sel, TEPh, Succ, Pout, ExpThen ),
     body_expand( Else, Pnxt, Eps, Sel, TEPh, Succ, Pout, ExpElse ).

body_expand( call(A), Pin, Eps, Sel, AllPaths, Succ, Pout, ExpCall ) :-
     !,
     ( A = Qual-Cable ->
          ( Qual == s ->
               %% ExpCall = (user:scall_1( Sel, Cable, Eps, AllPaths, Succ, ScPrb ),Pout is Pin * ScPrb)
               %% bb_get( pepl_module, LdMod ),
               ExpCall = (scall_1( Sel, Cable, Eps, AllPaths, Succ, ScPrb ),Pout is Pin * ScPrb)
                    % scall_1( all, Goal, Eps, Path, Succ, Prb ) :-
               ;
               %% ExpCall = (user:ns_call_1( Cable, Eps, Sel, Pin, AllPaths, Succ, Pout))
               ExpCall = (ns_call_1( Cable, Eps, Sel, Pin, AllPaths, Succ, Pout))
               % ExpCall = (user:ns_call_1( Cable, Eps, Sel, Pin, AllPaths, Succ ),Pout is Pin + ScPrb)
          )
          ;
          write( dont_know_how_to_deal_with_this_yet ), nl, abort
     ).

body_expand( (A;B), Pin, Eps, Sel, AllPaths, Succ, Pout, ExpITE ) :-
     !,
     % ExpITE = ((ExpA,\+ Succ==fail);ExpB),
     ExpITE = (ExpA;ExpB),
     body_expand( A, Pin, Eps, Sel, AllPaths, Succ, Pout, ExpA ),
     body_expand( B, Pin, Eps, Sel, AllPaths, Succ, Pout, ExpB ).
     % body_expand( C, Pin, Eps, Sel, AllPaths, Succ, Pout, ExpC ).
body_expand( Goal, Pin, Eps, Sel, AllPaths, Succ, Pout, ExpGoal ) :-
     ( Goal = (A,B) ->
          Tuple = true,
          body_expand( B, APout, Eps, Sel, BPaths, Succ, Pout, ExpB )
          ;
          Tuple = false,
          ExpB = true, BPaths = [], A = Goal
     ),
     A =.. [AName|AArgs],
     length( AArgs, AArity ),
     ( ( predicate_property(A,built_in),LoF=user;
          predicate_property(pepl_slp:A,imported_from(LoF)) ) -> 
          ( Tuple == true ->
               ExpGoal = (AllPaths=[0|BPaths],
                         LoF:A, APout is Pin, ExpB
                         )
                         ;
               ExpGoal = (AllPaths=[0], % Pout is Pin,
                         % if(LoF:AtmG,(Pout is Pin),(Pout is Pin,Succ=fail))
                         LoF:A, Pout is Pin
                         )
          )
          ;
          ( Tuple == true ->
               true
               ;
               Pout = APout
          ),
          TrA =.. [AName,IdA,ArcPin,Eps,Sel,APaths,Succ,APout|AArgs],
          bb_get( pepl_module, LdMod ),
          % ExpA = ( user:rc(Sel,AName/AArity,IdA,PrA),
          ExpA = ( LdMod:rc(Sel,AName/AArity,IdA,PrA),
                         ArcPin is Pin * PrA,
                         AllPaths = [APaths|BPaths],
                         TrA
                    ),
          ( Tuple == true ->
               ExpGoal = (ExpA, ( (Succ \== fail -> ExpB ; Pout is APout) ) )
               ;
               ExpGoal = ExpA
          )
     ).

/* 
body_expand( (A,B), Pin, Eps, Sel, AllPaths, Succ, Pout, ExpITE ) :-
     !,
     ExpITE = (AllPaths = [APh|BPh],
          if(ExpA,(Succ\==fail->ExpB;(BPh=[],Pout is PAo)),
                    % (BPh=[],Pout is Pin)
                    fail
                    )
          ),
     body_expand( A, Pin, Eps, Sel, APh, Succ, PAo, ExpA ),
     body_expand( B, PAo, Eps, Sel, BPh, Succ, Pout, ExpB ).

body_expand( AtmG, Pin, Eps, Sel, AllPaths, Succ, Pout, ExpBody ) :-
     AtmG =.. [AName|AArgs],
     length( AArgs, AArity ),
     ( ( predicate_property(AtmG,built_in),LoF=user;
          predicate_property(pepl_slp:AtmG,imported_from(LoF)) ) -> 
          % AllPaths = Paths,
          % Else = (Succ = fail, Pout=Pin),
          % ExpBody = ( if(LoF:A,true,Else), Pnxt is Pin, TrB )
          ExpBody = (AllPaths=[0],Pout is Pin,
                         % if(LoF:AtmG,(Pout is Pin),(Pout is Pin,Succ=fail))
                         LoF:AtmG
                         )
          ;
          TrAtmG =.. [AName,IdA,PinA,Eps,Sel,AAPaths,Succ,Pout|AArgs],
          ExpBody = ( user:rc(Sel,AName/AArity,IdA,PrA),
                         PinA is Pin * PrA,
                         AllPaths = [IdA|AAPaths],
                         if(TrAtmG,true,(Pout is PinA, Succ=fail))
                    )
     ).
     % TrB = (Pout is Pnxt, Paths=[] ).
*/

/* 
body_expand( Body, Pin, Eps, Sel, AllPaths, Succ, Pout, ExpBody ) :-
     ( Body = (A,B) -> Rec=y ; A = Body, Rec=n ),
     A =.. [AName|AArgs],
     length( AArgs, AArity ),
     ( ( predicate_property(A,built_in),LoF=user;
          predicate_property(A,imported_from(LoF)) ) -> 
          AllPaths = Paths,
          Else = (Succ = fail, Pout=Pin),
          % ExpBody = ( if(LoF:A,true,Else), Pnxt is Pin, TrB )
          ExpBody = ( if((LoF:A,Pnxt is Pin),TrB,Else))
          ;
          TrA =.. [AName,IdA,PinA,Eps,Sel,APath,Succ,Pnxt|AArgs],
          ExpBody = ( user:rc(Sel,AName/AArity,IdA,PrA),
                         PinA is Pin * PrA,
                         AllPaths = [APath|Paths],
                         TrA, 
                         (Succ==fail->Pout is Pnxt;TrB) )
     ),
     ( Rec == y -> 
          body_expand( B, Pnxt, Eps, Sel, Paths, Succ, Pout, TrB )
          ;
          TrB = (Pout is Pnxt, Paths=[] )
     ).
     */

% sprint( (myce(A,B):-some(C,D),silly(D,B)), ['A'=A,'B'=B,'X'=C,'Z'=D] ).
sprint( Clause, Vars ) :-
     ( Clause = (Head :- Body) -> 
          sprint_goal( Head, 1, Vars ),
          write( ' :-' ), nl, tab( 4 ),
          sprint_body( Body, 1, Vars )
          ;
          ( Clause = ( :- Directive ) ->
               write( ':- ' ), sprint_metas( Directive, 2, Vars )
               ;
               sprint_goal( Clause, 1, Vars ) 
          )
     ),
     write( '.' ), nl, nl.

sprint_body( Body, Ind, Vars ) :-
     (Body=(A,B) ->
          sprint_metas( A, Ind, Vars ),
          write( ',' ), nl, tab( Ind * 4 ),
          sprint_body( B, Ind, Vars )
          ;
          % tab( Ind*4 ),
          sprint_goal( Body, Ind, Vars )
     ).

sprint_goal( (If->Then;Else), Ind, Vars ) :-
     !,
     Ind1 is Ind + 1,
     write( '( ' ),
     sprint_metas( If, Ind1, Vars ),
     write( ' -> ' ), nl, tab( Ind1*4 ),
     sprint_body( Then, Ind1, Vars ),
     nl, tab( Ind1*4 ),
     write( ';' ), nl, tab( Ind1*4 ),
     sprint_body( Else, Ind1, Vars ), nl,
     tab( Ind*4 ),
     write( ')' ).
sprint_goal( (Either;Or), Ind, Vars ) :-
     !,
     write( '( ' ),
     Ind1 is Ind + 1,
     sprint_metas( Either, Ind1, Vars ), 
     write( ';' ), 
     sprint_metas( Or, Ind1, Vars ),
     write( ' )' ).

sprint_goal( if(If,Then,Else), Ind, Vars ) :-
     !,
     Ind1 is Ind + 1,
     write( 'if( ' ),
     sprint_metas( If, Ind1, Vars ),
     write( ',' ), nl, tab( Ind1*4 ),
     sprint_metas( Then, Ind1, Vars ),
     write( ',' ), nl, tab( Ind1*4 ),
     sprint_metas( Else, Ind1, Vars ),
     nl, tab( Ind*4 ),
     write( '  )' ).
sprint_goal( InGoal, _Ind, Vars ) :-
     InGoal =.. [Name|Args],
     subs_vars( Args, Vars, SubsArgs ),
     Goal =.. [Name|SubsArgs],
     write( Goal ).
     % write( Goal ).

subs_vars( [], _Vars, [] ).
subs_vars( [H|T], Vars, [SubH|SubT] ) :-
     ( var(H) ->
          ( eqpairs_v_id_match(Vars,SubH=H) ->
               true
               ;
               SubH = H
          )
          ;
          ( compound(H) ->
               H =.. [Hname|Hargs],
               subs_vars( Hargs, Vars, SubHargs ),
               SubH =.. [Hname|SubHargs]
               ;
               ( (number(H); H==[]) -> 
                    SubH = H
                    ;
                    atom_concat( H, '\'', RqH ),
                    atom_concat( '\'', RqH, SubH )
                    % SubH = H
               )
          )
     ),
     subs_vars( T, Vars, SubT ).

eqpairs_v_id_match( [Hk=Hv|T], K=V ) :-
     ( Hv==V -> 
          K = Hk
          ;
          eqpairs_v_id_match( T, K=V )
     ).

sprint_metas( Meta, Ind, Vars ) :-
     ( Meta \= (_A,_B) ->
          sprint_goal( Meta, Ind, Vars )
          ;
          write( '(' ),
          sprint_body( Meta, Ind, Vars ), nl,
          tab( Ind*4 ), write( ')' )
     ).
% 2014/1/28, we added this because we are dropping op :: priority 
% to 600 for logtalk
%
clause_parenthesis( ((Lbl::Head) :- Body)-What, (Lbl::(Head:-Body))-What ) :- !.
clause_parenthesis( ((Lbl::Head) --> Body)-What, (Lbl::(Head --> Body))-What ) :- !.
clause_parenthesis( Other, Other ).
