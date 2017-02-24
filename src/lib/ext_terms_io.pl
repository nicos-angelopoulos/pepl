% :-module( terms_io, [create_n_terms_file/3, make_n_terms/3, write_terms/2, load_terms/2,update_file_terms/2] ).

:- pl( sicstus(_A), ensure_loaded( library(system) )).  % file_exists/1

write_terms( MailFile, AllMailTerms ) :-
        open( MailFile, write, Stream ),
        write_terms_stream( Stream, AllMailTerms ),
        close( Stream ).

write_terms_stream( _Stream, [] ).
write_terms_stream( Stream, [H|T] ) :-
        write_canonical( Stream, H ),
        write( Stream, '.' ),
        nl( Stream ),
        write_terms_stream( Stream, T ).

load_terms( Filename, Terms ) :-
        % file_exists( Filename ),
        open( Filename, read, Stream ),
	   current_input( Inp ),
	   set_input( Stream ),
        ( load_terms( Terms ) ; true ),
	   set_input( Inp ),
        close( Stream ),
	   !.

load_terms( Terms ) :-
        read_term( Term, [variable_names(Vns)] ),
        ( Term == end_of_file   ->
                Terms = []
                        ;
                Terms = [Term-Vns|MoreTerms],
                load_terms( MoreTerms )
        ).
