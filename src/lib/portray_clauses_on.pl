:- ensure_loaded( library(portray_clauses) ).

portray_clauses_on( File, Clauses ) :-
     current_output( Old ),
     open( File, write, Out ),
     set_output( Out ),
	portray_clauses( Clauses ),
     set_output( Old ),
     close( Out ).
