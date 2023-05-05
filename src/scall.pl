
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

A more complex example:
==
?- sload_pe(doubles).

?- scall_sum( doubles(head), Prb ).
Prb = 0.25.

?- scall_sum( doubles(tail), Prb ).
Prb = 0.25.

?- scall_sum( doubles(Side), Prb ).
Prb = 0.5.
==

@author nicos angelopoulos
@version  0:1 2023/05/05

*/
scall_sum( Goal, Prb ) :-
     findall( DrvPrb, scall_1( all,Goal,0,_Path,true,DrvPrb), DPrbs ),
     sum_list( DPrbs, Prb ).

/** scall_findall( +Goal, -Pairs ).

Findall Instantiation-Prb pairs for derivations of stochastic Goal.

It also returns failed path probabilities. 
Instantiation is either the reserved token 'fail', or a term within Goal.
If Goal is a single, variable, argument term, then the value of the variable
is preserved. If Goal is of the form Left-Right, then Left is preserved
and Right is expected to be the callable Goal, otherwise the whole of Goal is preserved.

The order in Pair are according to standard SLD resolution. 
The only special feature is that probabilistic failures are returned.
There is no uniqueness on Instantion values.

==
?- sload_pe(doubles).
?- scall_findall( doubles(X), Pairs ).
Freqs = [head-0.25, fail-0.25, fail-0.25, tail-0.25].
==

==
?- sload_pe(member3).
?- scall_findall( X-member3(X,[a,b,c]), Pairs ).
Pairs = [a-1/3, b-0.2222222222222222, c-0.14814814814814814, fail-0.09876543209876543, fail-0.19753086419753085].
==

*/
scall_findall( GoalPrv, Freqs ) :-
     ( (GoalPrv=..[_,Rec],var(Rec)) -> Goal = GoalPrv; (GoalPrv = Rec-Goal -> true ; Rec = Goal )),
     findall( Tkn-Prb, (scall(Goal,_Path,Succ,Prb), (Succ==fail->Tkn=fail;Tkn=Rec)), Freqs ).

/** scall( Goal ).

Succeeds for all instantiations for which stochastic Goal has a successful derivation. 

This uses standard SLD resolution so the order is as per Prolog.
Failure paths are ignored here.

==
?- sload_pe(coin).
?- scall(coin(Flip)).
Flip = head ;
Flip = tail.

?- sload_pe(doubles).
?- scall(doubles(X)).
X = head ;
X = tail.
==

Compare to
==
?- scall_findall( doubles(X), Pairs ).
Pairs = [head-0.25, fail-0.25, fail-0.25, tail-0.25].
==

@author nicos angelopoulos
@version  0:2 2023/05/05, this used to be sampling based
@see scall/6 for full control
@see scall_findall/2, scall_sum/2.

*/
scall( Goal ) :-
     scall_1( all, Goal, 0, _Path, Succ, _Prb ),
     Succ \== fail. % fixme: or false ?

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
