simplify_mults_in_quotient( Nom, Dnm, Quotient ) :-
	simplify_mults_in_quotient( Nom, Dnm, _Snm, _Sdm, Quotient ).
	
simplify_mults_in_quotient( Nom, Dnm, Snm, Sdm, Quotient ) :-
	( (ground(Nom),Nom =:= 0) -> 
		Quotient is 0, Snm is 0, Sdm is 1
		;
	( (var(Nom);var(Dnm)) ->
	 ( Nom == Dnm -> 
	  Quotient is 1, Snm is 1, Sdm is 1
	  ;
	  Quotient = Nom / Dnm, Snm = Nom, Sdm = Dnm
	 )
	 ;
	 ( \+ \+ Nom = Dnm -> 
	  Quotient is 1, Snm is 1, Sdm is 1
	  ;
	  ( Nom = Anom * Bnom ->
	   simplify_mults_in_quotient( Anom, Dnm, DcmpAnom, DcmpAdnm, AQuotient ),
	   ( AQuotient == 1 -> 
	    if_ground_eval_else_unify( Bnom, Quotient ), Snm = Bnom, Sdm is 1
	    ;
	    ( ground(AQuotient) -> 
	     if_ground_eval_else_unify( AQuotient * Bnom, Quotient ),
		Snm = Bnom * AQuotient, Sdm is 1
	     ;
	     % AQuotient = DcmpAnom / DcmpAdnm,
	     simplify_mults_in_quotient( Bnom, DcmpAdnm, BdcNm, BdcDn, BQuotient ),
	     ( BQuotient == 1 -> 
	      if_ground_eval_else_unify( DcmpAnom, Quotient ),
		 Snm = DcmpAnom, Sdm is 1
	      ;
	      ( ground(BQuotient) ->
	       if_ground_eval_else_unify( DcmpAnom * BQuotient, Quotient ),
		  Snm = Anom * BQuotient, Sdm is 1
	       ;
	       Quotient = DcmpAnom * BQuotient,
		  Snm = DcmpAnom * BdcNm, Sdm = BdcDn
	      )
	     )
	    )
	   )
	   ;
	   ( Dnm = Adnm * Bdnm -> 
	    simplify_mults_in_quotient( Nom, Adnm, AdnmDcNm, AdnmDcDm, AQuotient ),
	    ( ground(AQuotient) ->
	     if_ground_eval_else_unify( AQuotient / Bdnm, Quotient ),
		Snm is AQuotient, Sdm = Bdnm
	     ;
	     simplify_mults_in_quotient( AdnmDcNm, Bdnm, BdnmDcNm, BdnmDcDm, _BQuotient ),
	     if_ground_eval_else_unify( BdnmDcNm / AdnmDcDm * BdnmDcDm, Quotient ),
		Snm = BdnmDcNm, Sdm = AdnmDcDm * BdnmDcDm
	    )
	    ;
	    if_ground_eval_else_unify( Nom / Dnm, Quotient ),
	    Snm = Nom, Sdm = Dnm
	   )
	  )
	 )
	)
	).
