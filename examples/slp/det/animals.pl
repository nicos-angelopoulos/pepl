% :- set(c,3)?
:- set(h,100)?
%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% class/2 learns the class (mammal/fish/reptile/bird) of various animals.
%	This has been extended due to a suggestion by James Cussens
%	on the use of probailistic information (see use of prob/4).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Mode declarations

:- modeh(1,class(+animal,#class))?
:- modeb(1,has_covering(+animal,#covering))?
:- modeb(1,has_legs(+animal,#nat))?
:- modeb(1,homeothermic(+animal))?
:- modeb(1,has_eggs(+animal))?
% :- modeb(1,not(has_milk(+animal)))?
% :- modeb(1,not(has_gills(+animal)))?
%:- modeb(1,nhas_gills(+animal))?
:- modeb(*,habitat(+animal,#habitat))?
:- modeb(1,has_milk(+animal))?
% :- modeh(1,false)?
:- modeb(1,class(+animal,#class))?
:- modeb(1,has_gills(+animal))?

% prune(H,B) :-
%	write((H:-B)), write(':  User_neg_cover='),
%	user_neg_cover(X),
%	write(X), nl,
%	fail.

user_hyp_size(1).

% user_pos_cover(X) :- pos_cover(X).
% user_neg_cover(X) :- neg_cover(X).
% user_hyp_size(X1) :- hypothesis(H,B,_), bagof(L,(in(L,B),L\=true),Bag),
%	length([H|Bag],X), write('Size '), write(B), write(' '), write(X), nl,
%	X1 is X+1.

% user_hyp_size(1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Types

animal(dog).  animal(dolphin).  animal(platypus).  animal(bat).
animal(trout).  animal(herring).  animal(shark). animal(eel).
animal(lizard).  animal(crocodile).  animal(t_rex).  animal(turtle).
animal(snake).  animal(eagle).  animal(ostrich).  animal(penguin).

class(mammal).  class(fish).  class(reptile).  class(bird).

covering(hair).  covering(none).  covering(scales).  covering(feathers).

habitat(land).  habitat(water).  habitat(air).  habitat(caves).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Background knowledge

has_covering(dog,hair).
has_covering(dolphin,none).
has_covering(platypus,hair).
has_covering(bat,hair).
has_covering(trout,scales).
has_covering(herring,scales).
has_covering(shark,none).
has_covering(eel,none).
has_covering(lizard,scales).
has_covering(crocodile,scales).
has_covering(t_rex,scales).
has_covering(snake,scales).
has_covering(turtle,scales).
has_covering(eagle,feathers).
has_covering(ostrich,feathers).
has_covering(penguin,feathers).

has_legs(dog,4).
has_legs(dolphin,0).
has_legs(platypus,2).
has_legs(bat,2).
has_legs(trout,0).
has_legs(herring,0).
has_legs(shark,0).
has_legs(eel,0).
has_legs(lizard,4).
has_legs(crocodile,4).
has_legs(t_rex,4).
has_legs(snake,0).
has_legs(turtle,4).
has_legs(eagle,2).
has_legs(ostrich,2).
has_legs(penguin,2).

has_milk(dog).
has_milk(dolphin).
has_milk(bat).
has_milk(platypus).


homeothermic(dog).
homeothermic(dolphin).
homeothermic(platypus).
homeothermic(bat).
homeothermic(eagle).
homeothermic(ostrich).
homeothermic(penguin).


habitat(dog,land).
habitat(dolphin,water).
habitat(platypus,water).
habitat(bat,air).
habitat(bat,caves).
habitat(trout,water).
habitat(herring,water).
habitat(shark,water).
habitat(eel,water).
habitat(lizard,land).
habitat(crocodile,water).
habitat(crocodile,land).
habitat(t_rex,land).
habitat(snake,land).
habitat(turtle,water).
habitat(eagle,air).
habitat(eagle,land).
habitat(ostrich,land).
habitat(penguin,water).

has_eggs(platypus).
has_eggs(trout).
has_eggs(herring).
has_eggs(shark).
has_eggs(eel).
has_eggs(lizard).
has_eggs(crocodile).
has_eggs(t_rex).
has_eggs(snake).
has_eggs(turtle).
has_eggs(eagle).
has_eggs(ostrich).
has_eggs(penguin).

has_gills(trout).
has_gills(herring).
has_gills(shark).
has_gills(eel).

nhas_gills(X) :- animal(X), not(has_gills(X)).

%%%%%%%%%%%%%%%%%%%%%%
% Definition of conditional probability.
%
% prob(Var,Pred,Cond,Prob)
%
% is true if Prob is the probability that Var satisfies
% Pred given that it satifies Cond.


prob(Var,Pred,Cond,undefined) :-
	not(Cond).

prob(Var,Pred,Cond,Prob) :-
	num(Var,(Pred,Cond),Int1),
	num(Var,Cond,Int2),
	Prob is (Int1+0.0)/(Int2+0.0).

num(Var,Pred,Num) :-
	setof(Var,Pred,L),
	length(L,Num).

% we can state P(mammal|animal) \in [0.1,0.2], as follows:

% :- prob(X,class(X,mammal),animal(X),P), (P<0.1 ; P>0.2).

animal(cat). animal(dragon).

animal(girl).
animal(boy).

has_milk(cat).
homeothermic(cat).

% :- [query]?

literalof(L,L) :- not(L=.. [','|_]).
literalof(L,(L,_)).
literalof(L,(_,Conj)) :- literalof(L,Conj).

% :- hypothesis(class(_,reptile),Body,_), literalof(has_legs(_,_),Body).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Positive examples

class(lizard,reptile).

class(dog,mammal).
class(dolphin,mammal).
class(platypus,mammal).
class(bat,mammal).

class(trout,fish).
class(herring,fish).
class(shark,fish).
class(eel,fish).

class(crocodile,reptile).
class(t_rex,reptile).
class(snake,reptile).
class(turtle,reptile).

class(eagle,bird).
class(ostrich,bird).
class(penguin,bird).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Negative examples

% :- class(X,mammal), class(X,fish).
% :- class(X,mammal), class(X,reptile).
% :- class(X,mammal), class(X,bird).
% :- class(X,fish), class(X,reptile).
% :- class(X,fish), class(X,bird).
% :- class(X,reptile), class(X,bird).

:- class(trout,mammal).
:- class(herring,mammal).
:- class(shark,mammal).
:- class(lizard,mammal).
:- class(crocodile,mammal).
:- class(t_rex,mammal).
:- class(turtle,mammal).
:- class(eagle,mammal).
:- class(ostrich,mammal).
:- class(penguin,mammal).
:- class(dog,fish).
:- class(dolphin,fish).
:- class(platypus,fish).
:- class(bat,fish).
:- class(lizard,fish).
:- class(crocodile,fish).
:- class(t_rex,fish).
:- class(turtle,fish).
:- class(eagle,fish).
:- class(ostrich,fish).
:- class(penguin,fish).
:- class(dog,reptile).
:- class(dolphin,reptile).
:- class(platypus,reptile).
:- class(bat,reptile).
:- class(trout,reptile).
:- class(herring,reptile).
:- class(shark,reptile).
:- class(eagle,reptile).
:- class(ostrich,reptile).
:- class(penguin,reptile).
:- class(dog,bird).
:- class(dolphin,bird).
:- class(platypus,bird).
:- class(bat,bird).
:- class(trout,bird).
:- class(herring,bird).
:- class(shark,bird).
:- class(lizard,bird).
:- class(crocodile,bird).
:- class(t_rex,bird).
:- class(turtle,bird).
