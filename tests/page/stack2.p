declare

  stk : ram (nat 8) (nat 8),
  x   : nat 8,
  y   : nat 8

in

  x := 67 ;
  y := 68 ;
  tick ;

  push stk x ;
  tick ;
  tick ;

  push stk y ;
  tick ;
  tick ;

  pop stk x ;
  tick ;
  tick ;

  pop stk y ;
  tick ;

  print x ; tick ; print y ; tick ;

  print x ; tick ; print y ; tick ;

  halt
