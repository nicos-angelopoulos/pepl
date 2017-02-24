:- ensure_loaded( library('compare_list_selects') ).	% /6.
:- ensure_loaded( library(system) ). % system/1.
:- ensure_loaded( library(pl) ). % /2.

pop_first_three_numbers_from_file( File, First, Sec, Third ) :-
	( pop_first_three_numbers_from_file_1( File, First, Sec, Third ) ->
		true
		;
		write( user_error, pop_failed_retrying ), nl( user_error ),
		pop_first_three_numbers_from_file( File, First, Sec, Third )
	).

pop_first_three_numbers_from_file_1( File, First, Sec, Third ) :-
	% Replen = library('experiment/proto_seeds'),
	pl( sicstus(3:9:_), absolute_file_name( library(proto_seeds), Replen, [access(exist)] ) ),
	pl( sicstus(3:8:_), absolute_file_name( library(proto_seeds), Replen) ),
	pl( yap(_), absolute_file_name( library(proto_seeds), Replen) ),
	Bounds = [30268,30306,30322],
	pop_first_n_bounded_numbers_from_file( File, Replen, Bounds, Numbers ),
	Numbers = [First, Sec, Third].

pop_first_n_bounded_numbers_from_file( File, Replen, Bounds, Numbers ) :-
	read_file_of_numbers_to_list( File, ListOfNums ),
	( compare_list_selects(Bounds,ListOfNums,=<,_Rj,Numbers, Rem) -> 
		open( File, write, OutStream ),
		write_list_to_single_stream( Rem, OutStream ),
		close( OutStream )
		;
		read_file_of_numbers_to_list( Replen, ReplenNums ),
		( ReplenNums = ListOfNums ->
			write( ['error >> in:pop_first_three_numbers_from_file/4, either ', File, ' has not enough numbers, or last 3 parameters are instatiated to the wrong atoms. Now failing .... (file not changed)'] ),
			fail
			;
			atom_concat( 'cp -f ', Replen, PrfxCp ),
			atom_concat( PrfxCp, ' ', MdfxCp ),
			atom_concat( MdfxCp, File, Cp ),
			write( user_error, replenishing_seeds( File,Replen ) ),
			nl( user_error ),
			system( Cp ),
			pop_first_n_bounded_numbers_from_file( File, Replen, Bounds, Numbers )
		)
	).

write_list_to_single_stream( [], _Stream ).
write_list_to_single_stream( [H|T], Stream ) :-
     write( Stream, H ), write( Stream, ' ' ),
	( T = [Sec,Trd|R] ->
        write( Stream, Sec ), write( Stream, ' ' ),
        write( Stream, Trd ), write( Stream, ' ' )
	   ;
	   R = T
	),
	nl(Stream),
	write_list_to_single_stream( R, Stream ).

read_file_of_numbers_to_list( File, ListOfNums ) :-
	open( File, read, InStream ),
	read_stream_of_numbers_to_list( InStream, ListOfNums ),
	close( InStream ).

read_stream_of_numbers_to_list( InStream, ListOfNums ) :-
	read_number( InStream, FirstNumb ), 
	!,
	ListOfNums = [FirstNumb|TailOfNums],
	read_stream_of_numbers_to_list( InStream, TailOfNums ).
read_stream_of_numbers_to_list( _AnyStream, [] ).

read_number( InStream, _Number ) :-
        at_end_of_stream( InStream ),
        !, 
        fail.

read_number( InStream, Number ) :-
        get0( InStream, Char ),
        ( (Char > 47, Char < 58) -> 
                Acc is Char - 48,
                read_number_1( InStream, Acc, Number )
                                 ;
                read_number( InStream, Number )
        ).

read_number_1( InStream, Acc, Number ) :-
        get0( InStream, Char ),
        !,
        ( (Char > 47, Char < 58) -> 
                NewAcc is (Acc * 10) + (Char-48),
                read_number_1( InStream, NewAcc, Number )
                                 ;
                Number is Acc 
        ).

