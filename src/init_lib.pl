% load system libraries, used across the pack

:- ensure_loaded(library(lists)).               % append/3, memberchk/2, member/2, select/3.
:- ensure_loaded( library(ordsets) ).
:- ensure_loaded(library(random)).              % random/1
:- ensure_loaded(library(system)).              % delete_file/1.

assert_lib_dir_if( Lib ) :-
	( (current_predicate(user:library_directory/1),
        % write( user_error, checking(Lib) ), nl( user_error ),
	   user:library_directory(Lib)) -> 
		true 
		; 
          % write( user_error, asserting(user:Lib) ), nl( user_error ),
		assertz( user:library_directory(Lib) ) 
	).

yap_start :-
	% use_module( library(lists) ),
	getcwd( D ),
	atom_codes( D, DCs ),
	append( DCs, "/lib", LibCs ),
	atom_codes( Lib, LibCs ),
	assert_lib_dir_if( Lib ),
	set_prolog_flag(verbose,silent).

sicstus_start :-
	absolute_file_name( 'lib/', Lib ),
	assert_lib_dir_if( Lib ),
	ensure_loaded( library(portray_no_compile) ).

swi_start :-
	% absolute_file_name( 'lib/', Lib ),
	prolog_load_context( directory, Dir ),
	atom_concat( Dir, '/lib', Lib ),
	% write( lib(Lib) ), nl,
	assert_lib_dir_if( Lib ),
	atom_concat( Dir, '/lib/swi', LibSwi ),
	assert_lib_dir_if( LibSwi ),
	ensure_loaded( library(swi_compat) ).

% load pack interface code
:- ensure_loaded(sample).                    % /1, /5.
:- ensure_loaded(scall).                     % /1, /2, /5.
:- ensure_loaded(seed_pe).                   % /0.

% load some basic pack code
:- ensure_loaded(estim).                     % /4.
:- ensure_loaded(resolution_pe).             % /6.
:- ensure_loaded(set_prior).                 % /1.
:- ensure_loaded(slp_file_location).         % /2.

% load pepl.pl specifics, squirelling these away from the doc server
%
:- ensure_loaded( library(datafile_to_frequencies) ).  % /4.
:- ensure_loaded( library(mold_vars_list) ).           % /2.
:- ensure_loaded( library(fam_setrand) ).              % /2.
:- ensure_loaded( library(pepl_messages) ).            % message/3.

:- ensure_loaded('lib/pl').
:- ensure_loaded('lib/requires_minimal').
:- ensure_loaded(library(lists)).

:- pl( sicstus(_A), sicstus_start ).
:- pl( yap(_A), yap_start ).
:- pl( swi(_A), swi_start ).

:- prolog_load_context( module, LdMod ),
   bb_put( pepl_module, LdMod ).
