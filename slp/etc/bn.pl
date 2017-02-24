:- use_module(library(ugraphs)).

% Definition of what a Bayes net is...

bn([],[]).
bn([RV|RVs],BN) :-                       %it's a Bayes net if
	bn(RVs,TailBN),                  %this is and ..
	connect(RV,RVs,TailBN,BN),       %we connect RV thus giving BN ...
	no_cycle_introduced(RV,BN).      %and this did not introduce a cycle.

connect(RV,RVs,TailBN,BN) :-
	choose_edges(RVs,RV,Edges),  %pbc choice
	add_edges(TailBN,Edges,BN).  %from library

choose_edges(RVs,RV,Edges) :-
	(RVs=[] ->
	    Edges=[];
	    choose_edges1(RVs,RV,Edges)
	).

choose_edges1([H|T],RV,[H-RV|Rest]) :-  %RV child of H
	choose_edges(T,RV,Rest).
choose_edges1([H|T],RV,[RV-H|Rest]) :-  %RV parent of H
	choose_edges(T,RV,Rest).
choose_edges1([_H|T],RV,Rest) :-         %no direct connection
	choose_edges(T,RV,Rest).

no_cycle_introduced(_RV,BN) :-         %no cycles if there is a top sort
	top_sort(BN,_).                %from library
