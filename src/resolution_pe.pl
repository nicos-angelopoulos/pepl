/** resolution_pe( +Method, +Goal, +Eps, -Path, -Succ, -Prb ).

Low level common interface for samping and probabilistic inference.

Method selects sample/top_sample (sampling) versus all (pbc inf).
Not sure what the difference of sample to top_sample is.


@author nicos angelopoulos
@version  0:2 2023/05/05, this was scall_1/6.
@see for sampling:  sample/1, sample/5.
@see for inference: scall/1, scall/2, scall/5, scall_sum/2, scall_findall/2.

*/
resolution_pe( sample, Goal, Eps, Path, Succ, Prb ) :-
     expand_sgoal( Goal, Spec, ClId, PrIn, Eps, sample, Path, Succ, Prb, ExpG ),
     rc( top_sample, Spec, ClId, PrIn ),
     ( call( pepl_slp:ExpG ) -> true ; Path = [ClId|_None4], Succ = fail, Prb is 0 ),
          % added in June 2001, for case when top sampling goal
          % is a non-stochastic one.
     !.
% scall_1( all, Goal, Eps, Path, Succ, Prb ) :-
resolution_pe( Other, Goal, Eps, Path, Succ, Prb ) :-
     expand_sgoal( Goal, Spec, ClId, PrIn, Eps, Other, Path, Succ, Prb, ExpG ),
     rc( Other, Spec, ClId, PrIn ),
     call( pepl_slp:ExpG ).
