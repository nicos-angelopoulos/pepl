s(1, A, B, C, [1|D], E, F, G, H) :-
	if(
		(
			[G,H]=[I,p],
			B<A,
			user:rc(C,p/1,J,K),
			L is A*K,
			D=[M|N],
			p(J,L,B,C,M,E,O,I),
			(	
				E==fail->true;
					user:rc(C,p/1,P,Q),
					R is O*Q,
					N=[S|T],
					p(P,R,B,C,S,E,U,I),
					(E==fail->true;F is U,T=[]))
		),
		true,
		(F is A,D=[],E=fail)
	).

