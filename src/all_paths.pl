:- ensure_loaded( init_lib ).
:- ensure_loaded( sload_pe ).

all_paths( File, Call ) :-
	sload_pe( File, [keep_pl(true)] ),
	E = -0.20,
	findall( Succ-FPath-Prb-Call, 
			(scall(Call,E,all,EPath,Succ,Prb),
                    write( prb(Prb) ), nl,
				flatten_nv(EPath,FPath)),
		AllPathsPrbs ),
	slisting,
	print_all_paths( AllPathsPrbs, c(0,0,0,0), Yks, c(T,S,F,P) ),
	% remove_duplicates( Yks, UnqYks ),
	% print_unq_yks( UnqYks, 0, L ),
	print_unq_yks( Yks, [], 0, L ),
	write_a( total_number_of_paths(T) ), 
	write_a( total_successes(S) ), 
	write_a( total_failures(F) ), 
	write_a( total_probability(P) ),
	write_a( number_of_unique_yks(L) ).

print_unq_yks( [], _, Length, Length ).
print_unq_yks( [H|T], Seen, Acc, L ) :-
	( memberchk( H, Seen ) ->
		write_a( already_seen(H) ),
		NxAcc is Acc,
		NxSeen = Seen
		;
		write_a( yield(H) ),
		NxAcc is Acc + 1,
		NxSeen = [H|Seen]
	),
	print_unq_yks( T, NxSeen, NxAcc, L ).

write_a( Term ) :- portray_clause( Term ).

print_all_paths( [], Counts, [], Counts ).
print_all_paths( [Hs-Hp-Hb-Hc|T], c(Ta,Sa,Fa,Pa), [Hc|Tcs], Counts ) :-
	Tn is Ta + 1,
	Pn is Pa + Hb,
	( var(Hs) ->
		True = true_,
		Sn is Sa + 1, Fn is Fa
		;
		True = Hs,
		Sn is Sa, Fn is Fa +1
	),
	write_a( path(True,Hb,Hp,Hc) ),
	print_all_paths( T, c(Tn,Sn,Fn,Pn), Tcs, Counts ).
