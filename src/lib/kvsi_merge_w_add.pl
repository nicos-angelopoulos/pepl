kvsi_multi_merge_w_add( MultFreqs, UniFreq ) :-
	kvsi_multi_merge_w_add( MultFreqs, [], UniFreq ).

kvsi_multi_merge_w_add( [], Acc, Acc ).
kvsi_multi_merge_w_add( [H|T], Acc, Uni ) :-
	kvsi_merge_w_add( H, Acc, NxAcc ),
	kvsi_multi_merge_w_add( T, NxAcc, Uni ).
	

kvsi_merge_w_add( [], Slist, Slist ) :- !.
kvsi_merge_w_add( List, [], List ) :- !.
kvsi_merge_w_add( [Hk-Hv|T], [Sk-Sv|Ts], [Hm|Tm] ) :-
        ( Hk < Sk -> 
                Hm = Hk-Hv,
                kvsi_merge_w_add( T, [Sk-Sv|Ts], Tm )
                ;
                ( Hk =:= Sk ->
                        Nv is Hv + Sv,
                        Hm = Hk-Nv,
                        kvsi_merge_w_add( T, Ts, Tm )
                        ;
                        Hm = Sk-Sv,
                        kvsi_merge_w_add( [Hk-Hv|T], Ts, Tm )
                )
        ).
