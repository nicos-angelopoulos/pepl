s([the,man,walks,the,dog],[]).
s([the,dog,walks,to,the,man],[]).
s([a,dog,hits,a,ball],[]).
s([the,man,walks,in,the,house],[]).
s([the,man,hits,the,dog],[]).
s([a,ball,hits,the,dog],[]).

s([a,man,hits,the,ball,at,the,dog],[]).
s([the,man,hits,the,ball,at,the,house],[]).
s([the,man,takes,the,dog,to,the,ball],[]).
s([a,man,takes,the,ball,to,the,house],[]).
s([the,dog,takes,the,ball,to,the,house],[]).
s([the,dog,takes,the,ball,to,the,man],[]).
s([the,man,hits,the,ball,to,the,dog],[]).
s([the,man,walks,the,dog,to,the,house],[]).

:- negative_examples.

s([a,dog,walks,the],[]).
s([a,man,walks,the],[]).
s([a,man,walks,the,walks],[]).
s([a,man,walks,the,house,a],[]).
s([a,man,walks,the,dog,at],[]).
s([the,man,walks,the,dog,to,the],[]).
s([the,dog],[]).
