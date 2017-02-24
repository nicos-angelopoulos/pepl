partial_call_map( PartialCallStr, Pname, Args, InPos, OutPos ) :-
     ( PartialCallStr = InPos+OutPos+PartialCall ->
          PartialCall =.. [Pname|Args]
          ;
          ( PartialCallStr = InPos+PartialCall ->
               PartialCall =.. [Pname|Args]
               ;
               InPos is 1,
               PartialCallStr =.. [Pname|Args]
          ),
          \+ var(Args),
          length(Args, Length),
          OutPos is Length + 1
     ).
