sample_refutation( Goal, Path, Prb ) :-
	sample( Goal, Path, Succ, Prb ),
	Succ \== fail.
	% also try, sample( Goal, Path, true, Prb )
	% refutational_path( Path ).

/** sample(Goal).

True iff Goal is a stochastic goal that can be refuted from the stochastic clauses in memory.

==
?- sload_pe(coin).
?- seed_pe.
?- sample(coin(Flip)).
Flip = head.
==

sample(Goal) is equivalent to sample(Goal,0,_Path,Succ,Prb) where Succ is not =|fail|= and Prb is not =|0|=.
==
?- seed_pe.
?- sample(coin(Flip),0,Path,Succ,Prb).
Flip = head,
Path = [1],
Prb = 0.5.
==

The probability with which refutations/branches are sampled are proportional to the probabilities on the
clauses. That is, sampling replaces SLD resolution with stochastic resolution.

If you have packs: mlu, b_real and Real.
==
?- lib(mlu).
?- sload_pe(coin).
?- seed_pe.
?- mlu_sample( scall(coin(Side)), 100, Side, Freqs ),
   mlu_frequency_plot( Freqs, [interface(barplot),outputs([svg]),las=2] ).
resolution_pe
Freqs = [head-53, tail-47].
==
Produces file: real_plot.svg which contains the barplot for 53 heads and 47 tails from 100 coin flipping experiments.

Note that sampling is distinct to calling a Goal for finding its refutations and total probabilities.
Sampling always takes the most general form of Goal and also returns failure paths. The idea is that 
if the underlying SLP defines a unique probability space summing up to 1, then each branch is sampled
proportionaly and failed branches are integral part of the space. 

The above is particularly important if Goal is partially instantiated.
==
?- seed_pe.
?- sample(coin(tail)).
false.
?- seed_pe.
?- sample(coin(head)).
true.
?- seed_pe.
?- sample(coin(Flip)).
Flip = head.
==

To demonstrate the inability of SLPs to operate over arbitrary length objects, check:
==
?- sload_pe(member3).
?- lib(mlu).
?- seed_pe.
?- mlu_sample( sample(member3(X,[a,b,c])), 100, X, Freqs ),
   mlu_frequency_plot( Freqs, [interface(barplot),outputs(png),stem('meb3from3'),las=2] ).
Freqs = [a-31, b-20, c-22, fail-27].
==
Produces file: meb3from3.png

[[doc/html/images/meb3from3.png]]

...and: 
==
?- seed_pe.
?- mlu_sample( sample(member3(X,[a,b,c,d,e,f,g,h])), 100, X, Freqs ),
   mlu_frequency_plot( Freqs, [interface(barplot),outputs(png),stem('meb3from8'),las=2] ),
   write( freqs(Freqs) ), nl.

freqs([a-34,b-16,c-22,d-5,e-9,f-6,fail-2,g-3,h-3])
==
Produces file: meb3from8.png

[[doc/html/images/meb3from8.png]]

==

@author nicos angelopoulos
@version  0:1 2023/05/04
@see sample/5, for full control of sampling stochastic goals
@see scall/1, scall/5, scall_sum/2 and scall_sum/5 for finding the probability of refutations and goals.

*/
sample( Goal ) :-
     sample( Goal, 0, _Path, Succ, Prb ),
     Succ \== fail,
     Prb =\= 0.

/** sample(+Goal, +Eps, -Path, -Succ, -Prb).

True iff Goal is a stochastic goal that can be sampled from the stachastic clauses in memory.

Eps is the epsilon value below which a derivation is considered a failure (prunes low probability branches).
Path is the arithmetic index of the clauses used in the derivation. Succ is bound to =|false|= if this was
a failure branch and is unbound otherwise. Prb is the probability of the sampled branch.

This predicate implements probabilistic sampling. Instead of SLD resolution we use the probabilistic 
labels to sample from the tree. There is no backtracing, and probabilistic failures will be returned.

==
?- sload_pe(coin).
?- seed_pe.
?- sample(coin(Flip),0,Path,Succ,Prb).
Flip = head,
Path = [1],
Prb = 0.5.
==

The probability with which refutations/branches are sampled are proportional to the probabilities on the
clauses. That is, sampling replaces SLD resolution with stochastic resolution.

Note that sampling is distinct to calling a Goal for finding its refutations and total probabilities.
Sampling always takes the most general form of Goal and also returns failure paths. The idea is that 
if the underlying SLP defines a unique probability space summing up to 1, then each branch is sampled
proportionaly and failed branches are integral part of the space. 

The above is particularly important if Goal is partially instantiated.
==
?- seed_pe.
?- sample(coin(tail),0,Path,Succ,Prb).
Path = [1],
Succ = fail,
Prb = 0.5.
?- seed_pe.
?- sample(coin(head),0,Path,Succ,Prb).
Path = [1],
Prb = 0.5
?- seed_pe.
?- sample(coin(Flip),0,Path,Succ,Prb).
Flip = head,
Path = [1],
Prb = 0.5.
==

@author nicos angelopoulos
@version  0:1 2023/05/04
@see scall/1, scall/2, scall/5, scall_sum/2 and scall_findall/2 for finding the probability of refutations and goals.

*/
sample( Goal, Eps, Path, Succ, Prb ) :-
     expand_sgoal( Goal, Spec, ClId, PrIn, Eps, sample, Path, Succ, Prb, ExpG ),
     rc( top_sample, Spec, ClId, PrIn ),
     ( call( pepl_slp:ExpG ) -> true ; Path = [ClId|_None4], Succ = fail, Prb is 0 ),
          % added in June 2001, for case when top sampling goal
          % is a non-stochastic one.
     !.

/* 23.05.04: 
     I think these are equivalent older definitions, eg manully expanding the goal 

sample( Goal, FlatPath, Succ, Prb ) :-
	sample( Goal, 0, FlatPath, Succ, Prb ).
sample( Goal, Eps, FlatPath, Succ, Prb ) :-
	% functor( Goal, Name, Arity ),
	Goal =.. [Name|NmArgs],
	length( NmArgs, NmArity ),
	( Name/NmArity = phrase/3 ->
		NmArgs = [RealGoal|PhraseArgs],
		RealGoal =.. [RealName|RealArgs],
		append( RealArgs, PhraseArgs, Args ),
		length( Args, RealArity )
		;
		RealName = Name, RealArity is NmArity,
		Args = NmArgs
	),
	rc( sample, RealName/RealArity, ClId, Pr0 ),
	ExpGoal =.. [RealName,ClId,Pr0,Eps,sample,Path,Succ,Prb|Args],
	call( pepl_slp:ExpGoal ),
	flatten_nv( Path, FlatPath ).
*/

/* 23.05.04: 
     haven't checked these recently
*/
most_likely_deduction( Goal, MaxDedu, MaxPrb ) :-
	Goal =.. [Name|Args],
	length( Args, Arity ),
	rc( max, Name/Arity, ClId, Pr0 ),
	ExpGoal =.. [Name,ClId,Pr0,max,MaxDedu,MaxPrb|Args],
	pepl_slp:ExpGoal.

most_probable_deduction( Goal, MaxDedu, MaxPrb ) :-
	descending_probability_deductions( Goal, MaxDedu, MaxPrb ),
	!.

descending_probability_deductions( Goal, Dedu, Prb ) :-
	Goal =.. [Name|Args],
	length( Args, Arity ),
	ExpGoal =.. [Name,ClId,Pr0,all,Path,Prb|Args],
	findall( Prb-(Path-Goal), 
		( 
		   rc( all, Name/Arity, ClId, Pr0 ),
		   pepl_slp:ExpGoal
		),
		RefPairs 

		   ),
	keysort( RefPairs, Ascends ),
	reverse( Ascends, Descents ),
     member( Prb-(Dedu-Goal), Descents ).

descending_probability_resolutions( Goal, Ref, Prb ) :-
	descending_probability_deductions( Goal, Ref, Prb ),
	refutational_path( Ref ). 

most_probable_resolution( Goal, MaxRef, MaxPrb ) :-
	descending_probability_resolutions( Goal, MaxRef, MaxPrb ),
	!.

refutational_path( Path ) :-
	flatten_nv( Path, FlatPath ),
	last( FlatPath, Last ),
	Last \== fail.
