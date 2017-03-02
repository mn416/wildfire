declare

  stk : ram (nat 8) (nat 8),
  x   : nat 8,
  y   : nat 12

in

  x := 65 ;
  y := 66 ;
  tick ;
  push stk x ;
  tick ;
  push stk y ;
  tick ;
  pop stk y ;
  tick ;
  x := bits 7 to 0 of y ;
  tick ;
  print x
