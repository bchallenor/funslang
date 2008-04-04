\()()() ->
  let f a = (a < 7, a+1) in
  
  [iterate f 10 0, 42, 42, 42]