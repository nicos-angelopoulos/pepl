:- multifile portray_message/2.

portray_message( informational, Mess ) :-
        ( (  Mess=loading(_A,_B,_C)
	      ; Mess=loaded(_V,_W,_X,_Y,_Z,_G)
		 ; Mess=imported(_K,_L,_M)
		 ; Mess=foreign_resource(_P,_O,_S,_T)
		 ) ->
                true
                ;
                fail
        ).
