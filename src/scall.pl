
/** scall_sum( +Goal, -Prb ).

Prb is the sum of probabilities for all refutations of Goal- which should be a stochastic goal.

==
?- sload_pe(coin).
?- set_random(seed(101)).
?- scall_sum( coin(Flip), Prb ).
Prb = 1.0.

?-  scall_sum( coin(head), Prb ).
Prb = 0.5.

?- scall_sum( coin(tail), Prb ).
Prb = 0.5.
==

@author nicos angelopoulos
@version  0:1 2023/05/05

*/
scall_sum( Goal, Prb ) :-
     findall( DrvPrb, scall_1( all,Goal,0,_Path,true,DrvPrb), DPrbs ),
     sum_list( DPrbs, Prb ).

scall_1( sample, Goal, Eps, Path, Succ, Prb ) :-
     expand_sgoal( Goal, Spec, ClId, PrIn, Eps, sample, Path, Succ, Prb, ExpG ),
     rc( top_sample, Spec, ClId, PrIn ),
     ( call( pepl_slp:ExpG ) -> true ; Path = [ClId|_None4], Succ = fail, Prb is 0 ),
          % added in June 2001, for case when top sampling goal
          % is a non-stochastic one.
     !.
% scall_1( all, Goal, Eps, Path, Succ, Prb ) :-
scall_1( Other, Goal, Eps, Path, Succ, Prb ) :-
     expand_sgoal( Goal, Spec, ClId, PrIn, Eps, Other, Path, Succ, Prb, ExpG ),
     rc( Other, Spec, ClId, PrIn ),
     call( pepl_slp:ExpG ).
