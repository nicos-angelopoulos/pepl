
:- use_module( library(os) ).
% :- requires( os_term/2 ).
:- use_module( library(options) ).
% :- requires( to_list/2 ).

% :- requires( file_name_extends/3 ).

read_terms( Filename, Terms ) :-
     read_terms( Filename, Terms, [store(all)] ).

%% read_terms( +File, -Terms ).
%% read_terms( +File, -Terms, +Opts ).
%
% Read terms from a file.
%
% Opts =   
%	* match([FuncOrTerm-PartialCall|FoTPCPrs]),
%     * store(Store), in {*matched*, all}
%     * tail(Tail), a variable, default is [].
%
% example of storing terms to another file :
%  fixme: haven't tested if pldoc renders this properly
%  BE CAREFUL i changed the order of Terms and Opts on 2013/13/4

read_terms( Fname, Terms, Opts ) :-
	atom( Fname ),
	!,
	read_terms_1( Fname, Opts, Terms ).

read_terms( FnameIn, Terms, Opts ) :-
	os_term( FnameAtom, FnameIn ),
	!,
	read_terms_1( FnameAtom, Opts, Terms ).
read_terms( FnameIn, Terms, Opts ) :-
	absolute_file_name( FnameIn, Fname ),
	read_terms_1( Fname, Opts, Terms ).

read_terms_1( Fname, Opts, Terms ) :-
     ( memberchk(match(MatchItm),Opts) ->
          en_list( MatchItm, MatchList ),
		requires( map_list_tail/4 ),
          map_list_tail( MatchList, 4, partial_call_map, Match )
          ;
          Match = []
     ),
     ( memberchk(store(Store), Opts) ->
          true
          ;
          Store = true
     ),
     ( memberchk(tail(Tail), Opts) ->
          true
          ;
          Tail = []
     ),
	open( Fname, read, Stream ),
	read( Stream, Term ),
     read_stream_terms( Term, Match, Store, Stream, Tail, Terms ),
	close( Stream ).

read_stream_terms( end_of_file, _Match, _Store, _In, Tail, Terms ) :-
     !,
     Tail = Terms.
read_stream_terms( Term, Match, Store, In, Tail, Terms ) :-
     read_stream_terms_matches_term( Match, Term, OutTerm ),
     !,
     ( store_matched(Store) ->
          Terms = [OutTerm|TTerms]
          ;
          Terms = TTerms
     ),
	read( In, NxTerm ),
     read_stream_terms( NxTerm, Match, Store, In, Tail, TTerms ).
read_stream_terms( Term, Match, Store, In, Tail, Terms ) :-
     ( Store == all ->
          Terms = [Term|TTerms]
          ;
          TTerms = Terms
     ),
	read( In, NxTerm ),
     read_stream_terms( NxTerm, Match, Store, In, Tail, TTerms ).

read_stream_terms_matches_term( [[Pname,Args,InPos,OutPos]|_T], Term, OutTerm ) :-
     nths_add( [InPos,OutPos], Args, [Term,OutTerm], PArgs ), 
     Goal =.. [Pname|PArgs],
     call( Goal ),
     !.
read_stream_terms_matches_term( [_H|T], Term, OutTerm ) :-
     read_stream_terms_matches_term( T, Term, OutTerm ).

store_matched( all ).
store_matched( matched ).

/*
read_terms( Filename, Terms ) :-
	open( Filename, read, Stream ),
	read( Stream, Term ),
	read_stream_terms( Term, Stream, Terms ),
	close( Stream ).

read_stream_terms( end_of_file, _Stream, [] ) :-
	!.
read_stream_terms( Term, Stream, [Term|TTerms] ) :-
	read( Stream, NxTerm ),
	read_stream_terms( NxTerm, Stream, TTerms ).
     */
