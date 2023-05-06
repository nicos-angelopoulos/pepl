
/** seed_pe.

Set random seed to a standard location.

A convenience predicate for running the examples from a common starting point for the random seed.

Specifically it unfolds to
==
?- set_random(seed(101)).
==

==
?- sload_pe(coin).
?- seed_pe.
?- sample(coin(Flip)).
Flip = head.

?- set_random(seed(101)).
?- sample(coin(Flip)).
Flip = head.

?- sample(coin(Flip)).
Flip = tail.
==

@author nicos angelopoulos
@version  0.1 2023/05/05

*/

seed_pe :-
     set_random(seed(101)).
