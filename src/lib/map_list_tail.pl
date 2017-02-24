:- requires( partial_call_map/5 ).
:- requires( nths_add/4 ).

/*
map_list_tail( [append([f]),2:append(g)], 4, partial_call_map, Out ).
map_list_tail( [append([f]),2:append(g)], 3, partial_call_sieve, Out ).
*/

map_list_tail( InList, Length, PartialCallStr, OutList ) :-
	% Goal =.. [Functor,H,ProcH],
     partial_call_map( PartialCallStr, Pname, Args, InPos, _OutPos ),
     ( (Args == [],InPos =:= 1) ->
          NofArgs is Length + 1,
          map_list_tail_no_args( InList, NofArgs, Pname, OutList )
          ;
          length( Args, XtraArgsLength ),
          NofArgs is Length + 1 + XtraArgsLength,
          map_list_tail_ground_args( InList, NofArgs, Pname, Args, [InPos], OutList )
     ).

map_list_tail_no_args( [], _NofArgs, _Pname, [] ).
map_list_tail_no_args( [Hin|Tin], NofArgs, Pname, [Hout|Tout] ) :-
     functor( Goal, Pname, NofArgs ),
     Goal =.. [Pname,Hin|Hout],
	call( Goal ),
     map_list_tail_no_args( Tin, NofArgs, Pname, Tout ).

% Untested:
map_list_tail_ground_args( [], _NofArgs, _Pname, _Args, _Nths, [] ).
map_list_tail_ground_args( [Hin|Tin], NofArgs, Pname, Args, Nths, [Hout|Tout ] ) :-
     nths_add( Nths, Args, [Hin], NthsArgsPfx ),
     append( NthsArgsPfx, Hout, NthsArgs ),
     functor( Goal, Pname, NofArgs ),
     Goal =.. [Pname|NthsArgs],
     call( Goal ),
     map_list_tail_ground_args( Tin, NofArgs, Pname, Args, Nths, Tout ).
