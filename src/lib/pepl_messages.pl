:- multifile prolog:message/3.

pepl_error( Mess ) :-
    print_message( error, pepl(Mess) ),
    abort.
pepl_warn( Mess ) :-
    print_message( error, pepl(Mess) ).

prolog:message( pepl(Message) ) -->
     message(Message).

message( unknown_predicate(Spec) ) -->
     ['Unknown SLP predicate: ~w' - [Spec] ].

message( nothing_in_memory ) -->
    ['No SLP currently in memory.' - []].

message( skipping_datum(Goal,H) ) -->
    [['Skipping incompatible datum: ~w for goal: ~w'] - [Goal,H]].

message( data_format_error(Inner) ) -->
    ['Data format error: ~w' - [Inner]].

message( mixed_clause ) -->
    ['Mixed clause while translating SLP.'-[]].

message( dixed_clause(H,Term) ) -->
    ['Dixed clause, while translating SLP, with head: ~w and term: ~w.'-[H,Term]].

message( off_the_cliff ) -->
    ['Run out of K-V pairs while stripping on P.'-[]].

message( off_the_rocking_horse(Hk,End) ) -->
    ['Key value of head: ~w, matched end value: ~w'-[Hk,End]].

% informational
message( sload_src(SlpF) ) -->
    ['Sourcing SLP from: ~p'-[SlpF]].
message( sload_file(TmpF) ) -->
    ['Loading transformed SLP from: ~p'-[TmpF]].
