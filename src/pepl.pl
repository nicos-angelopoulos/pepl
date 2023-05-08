/** <module> An implementation of the FAM algorithm.

Pepl is an implemention of the failure adjusted (FAM) algorithm which does 
parameter estimation (PE) of the probability labels of stochastic logic programs (SLPs).

See documentation fam/1 for details on how to run parameter estimation on SLPs.

Example stochastic programs are in directory =|slp|= and example run scripts are in =|examples|=.

Licence
---
This software is distributed under the MIT licence.

## Installation and testing ...

Pepl runs on current versions of SWI (7) and Yap (6.3).

### ... on SWI

==
pack_install(pepl).
[library(pepl)].
[pack('pepl/examples/main')].
main.
==

### ... on Yap

Download latest sources from http://stoics.org.uk/~nicos/sware/pepl
or https://github.com/nicos-angelopoulos/pepl

==
gunzip pepl-*tgz
tar xf pepl-*tar
cd pepl-*
cd examples
yap
[main].
main.
==

## Resolution
In addition to parameter estimation Pepl implements two way of performing resolution over SLPs:
stochastic sampling resolution and SLD-based probabilisic inference.

### Stochastic sampling resolution

These predicates allow to sample from a loaded stochastic logic program (Slp). The resolution strategy here
are that of chosing between probabilistic choices according to their relative values. The main idea is that
when sampling many times from a top goal will in the long run sample each derivation path in proportion 
to the probability of the derivation. The probability of a derivation/refutation, is simply the product 
of all the probabilities attached to resolution steps during the derivation. 


For very deep probabilistic programs, it is sometimes useful to but a minimum value of probability
we are interested in. This is a way to shorten the search space without losing significant amounts
of probability mass (see second argument of sample/5).

See
  * sample/1
  * sample/5

### SLD-based probabilisic inference

These predicates allow standard SLD exploration of a stochastic query against an SLP. Predicates here
allow to explore what is derivable and often attach a probability and ather information to each derivation.

Note that in probabilistic inference we often are more interested in failures than in standard LP. 
This is because there is a probability mass loss which each failed probabilistic branch.

Probabilistic inference predicates
  * scall/1
  * scall/2
  * scall/5
  * scall_findall/2
  * scall_sum/2

## Predicates index

  * fam/1
  * sample/1, sample/5
  * scall/1, scall/2, scall/5
  * scall_findall/2, scall_sum/2
  * sload_pe/1, sload_pe/2
  * ssave/1, sls/0
  * seed_pe/0
  * switch_dbg/1, dbg_pepl/1
  * pepl_citation/2, pepl_version/2

## Package information

@author Nicos Angelopoulos
@license This software is distributed under the MIT licence
@version 2.3, 2023/5/6, added extensive sampling and inference support
@version 2.2, 2022/1/2
@version 2.1, 2017/2/25
@version 2.0.6, 2014/01/28 
@see  the user guide at pack('pepl/doc/pepl-user_guide.pdf').
@see James Cussens. Parameter estimation in stochastic logic programs. Machine Learning, 44(3):245-271, 2001. ftp://ftp.cs.york.ac.uk/pub/aig/Papers/james.cussens/jcslpmlj.pdf
@see Nicos Angelopoulos, Notes on the implementation of FAM, 3rd Probabilistic Logic Programming workshop (a ILP 2016 workshop), 03/09/2016, http://ceur-ws.org/Vol-1661/paper-05.pdf
@see pepl website http://stoics.org.uk/~nicos/sware/pepl
*/

%% pepl_citation( -Atom, -Bibterm ).
%
% This predicate succeeds once for each publication related to this library.
% Atom is the atom representation
% suitable for printing while Bibterm is a bibtex(Type,Key,Pairs) term of the same publication.
% Produces all related publications on backtracking.
%
pepl_citation( Atom, bibtex(Type,Key,Pairs) ) :-
     Atom = 'Notes on the implementation of FAM\nNicos Angelopoulos\n3rd Probabilistic Logic Programming workshop (PLP 2016, a workshop of ILP 2016). September 2016, Imperial College London. Pages 46-58.',
    Type = inproceedings,
    Key  = 'AngelopoulosN+2016',
    Pairs = [
               author = 'Nicos Angelopoulos',
               title  = 'Notes on the implementation of FAM',
               booktitle = '3rd Probabilistic Logic Programming Workshop (collocated with ILP 2016)',
               year = 2016,
               month = 'September',
               address = 'Imperial College, London',
               publisher = 'CEUR',
               volume   = '1661',
               url     = 'http://ceur-ws.org/Vol-1661/'
     ].



/** pepl_version( -Version, -Date ).

Pepl's current Version (Maj:Min:Fix) and publication date (date(Year,Month,Day)).

==
?- pepl_version(V,D).
V = 2:3:0,
D = date(2021, 5, 6).
==

@version 2:3:0 2023/05/06
@version 2:2:0 2021/01/01

*/
pepl_version( 2:3:0, date(2023,5,6) ).
