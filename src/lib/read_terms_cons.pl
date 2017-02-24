read_terms_cons( Filename, Cons, Opts, OptKeep, Terms ) :-
     open( Filename, read, Stream ),
     current_input( Inp ),
     set_input( Stream ),
     copy_term( OptKeep-Opts, OptKeepPrm-OptsPrm ),
	read_term( Term, OptsPrm ),
     KeepTerm =.. [Cons, Term, OptKeepPrm],
	read_terms_cons_1( Term, KeepTerm, Cons, Opts, OptKeep, Terms ),
     set_input( Inp ),
     close( Stream ).
     
read_terms_cons_1( end_of_file, _, _C, _Os, _OK, [] ) :-
     !.
read_terms_cons_1( _, KpTerm, Cons, Opts, OptKeep, [KpTerm|T] ) :-
     copy_term( OptKeep-Opts, OptKeepPrm-OptsPrm ),
	read_term( NxRead, OptsPrm ),
     NxKpTerm =.. [Cons, NxRead, OptKeepPrm],
     read_terms_cons_1( NxRead, NxKpTerm, Cons, Opts, OptKeep, T ).

