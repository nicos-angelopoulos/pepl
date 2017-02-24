bb_put( PrvKey, New ) :-
     to_swi_nb_key_value( PrvKey, Key ),
     ( ground(New) -> Type = g; Type = ng ),
	nb_setval( Key, Type/New ).
bb_get( PrvKey, Current ) :-
     to_swi_nb_key_value( PrvKey, Key ),
     nb_current( Key, Type/NbCurrent ),
     ( Type == g ->
          Current = NbCurrent
          ;
          copy_term( NbCurrent, Current )
     ).
bb_delete( PrvKey, Value ) :-
     to_swi_nb_key_value( PrvKey, Key ),
	nb_current( Key, _Type/Value ),
     nb_delete( Key ).

to_swi_nb_key_value( PrvKey, Key ) :-
     ( atom(PrvKey) ->
          Key = PrvKey
          ;
          ( number(PrvKey) ->
               atom_number( Key, PrvKey )
               ;
               term_to_atom( PrvKey, Key )
          )
     ).

file_exists( File ) :- 
	exists_file( File ).

variant( A, B ) :-
	A =@= B.

remove_duplicates( List1, List2 ) :-
	list_to_set( List1, List2 ).

host_name( Host ) :-
     ( getenv( 'HOSTNAME', Host ) ->
          true
          ;
          ( getenv( 'HOST', Host ) ->
               true
               ;
               Host = localhost
          )
     ).

datime( datime(Yr,Mo,Da,Hr,Mi,Se) ) :-
	get_time( Time ),
	convert_time( Time, Yr, Mo, Da, Hr, Mi, Se, _ ).

nth( A, B, C ) :-
	nth1( A, B, C ).

environ( Name, Value ) :-
	(var(Value) -> 
		getenv( Name, Value )
		;
		setenv( Name, Value )
	).
