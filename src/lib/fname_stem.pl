:- ensure_loaded( basename ). 		% /2.
:- ensure_loaded( library(lists) ).	% append/3

fname_stem( Fname, ExtCs, Stem, SurelyExtFname ) :-
	basename( Fname, BaseName ),
	atom_codes( BaseName, BnameCs ),
	( append( StemCs, ExtCs, BnameCs ) ->
		atom_chars( Stem, StemCs ),
		SurelyExtFname = Fname
		;
		Stem = BaseName,
		atom_codes( Fname, FnameCs ),
		append( FnameCs, ExtCs, SurelyExtFnameCs ),
		atom_chars( SurelyExtFname, SurelyExtFnameCs )
	).
