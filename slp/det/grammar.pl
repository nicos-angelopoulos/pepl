%%%%%%%%%%%%%%%%%%%%
% Grammar learning problem. Learns s/2, a simple English language
%	phrase grammar.

:- modeh(1,s(+wlist,-wlist))?
:- modeb(1,det(+wlist,-wlist))?
:- modeb(*,np(+wlist,-wlist))?
:- modeb(*,vp(+wlist,-wlist))?
:- modeb(1,prep(+wlist,-wlist))?
:- modeb(1,noun(+wlist,-wlist))?
:- modeb(1,verb(+wlist,-wlist))?
:- set(i,5)?
:- set(c,7)?

%%%%%%%%%%%%%%%%%%%%
% Types

wlist([]).
wlist([W|Rest]) :- constant(W), wlist(Rest).

%%%%%%%%%%%%%%%%%%%%%%%
% Background knowledge

np(S1,S2) :- det(S1,S3), noun(S3,S2).

det([a|S],S).
det([the|S],S).

vp(S1,S2) :- verb(S1,S2).
vp(S1,S2) :- verb(S1,S3), prep(S3,S2).

noun([man|S],S).
noun([dog|S],S).
noun([house|S],S).
noun([ball|S],S).

verb([takes|S],S).
verb([walks|S],S).
verb([hits|S],S).

prep([at|S],S).
prep([to|S],S).
prep([on|S],S).
prep([in|S],S).

%%%%%%%%%%%%%%%%%%%%
% Positive examples

s([the,man,walks,the,dog],[]).
s([the,dog,walks,to,the,man],[]).
s([a,dog,hits,a,ball],[]).
s([the,man,walks,in,the,house],[]).
s([the,man,hits,the,dog],[]).
s([a,ball,hits,the,dog],[]).

% More complex positive examples.

s([a,man,hits,the,ball,at,the,dog],[]).
s([the,man,hits,the,ball,at,the,house],[]).
s([the,man,takes,the,dog,to,the,ball],[]).
s([a,man,takes,the,ball,to,the,house],[]).
s([the,dog,takes,the,ball,to,the,house],[]).
s([the,dog,takes,the,ball,to,the,man],[]).
s([the,man,hits,the,ball,to,the,dog],[]).
s([the,man,walks,the,dog,to,the,house],[]).

%%%%%%%%%%%%%%%%%%%%
% Negative examples

:- s([a,dog,walks,the],[]).
:- s([a,man,walks,the],[]).
:- s([a,man,walks,the,walks],[]).
:- s([a,man,walks,the,house,a],[]).
:- s([a,man,walks,the,dog,at],[]).
:- s([the,man,walks,the,dog,to,the],[]).
:- s([the,dog],[]).
