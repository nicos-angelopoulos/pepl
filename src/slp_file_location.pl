:- ensure_loaded( library(filesex) ). % directory_file_path/3.

slp_data_file_location( File, FileName ) :-
	slp_files_location( File, ".pl",  FileName ).

slp_file_location( File, FileName ) :-
	( slp_files_location( File, slp,  FileName ) ->
		true
		;
		( slp_files_location( File, pl,  FileName ) ->
			true
			;
			( slp_files_location( File, '', FileName ) ->
				true 
				;
				FileName = File
			)
		)
	).

slp_files_location( File, Ext, Filename ) :-
	( file_or_ext_exists(File,Ext,Filename) ->
		true
		;
		( ( directory_file_path(slp,File,DirFile),
		    file_or_ext_exists(DirFile,Ext,Filename) ) ->
				true
				;
				( (absolute_file_name(slp(File),Slp),
				   file_or_ext_exists(Slp,Ext,Filename) ) ->
						true
						;
						directory_file_path( 'pepl/slp', File, Pfile ),
						Opts4 = [access(read),extensions(['',slp])],
						( (absolute_file_name(pack(Pfile),Pack,Opts4),
				             file_or_ext_exists(Pack,Ext,Filename) ) ->
								true
								;
								false
						)
				)
		)
	).

file_or_ext_exists( File, _Ext, Filename ) :-
	exists_file( File ),
	!,
	Filename = File.
file_or_ext_exists( File, Ext, Filename ) :-
	file_name_extension( File, Ext, Filename ),
	exists_file( Filename ).
