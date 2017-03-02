declare

  ret : label,
  r   : label,
  stk : ram (nat 8) (nat 8),
  x   : nat 8

in

  x := 65 ;
  ret := #prRet ;
  tick ;
  push stk ret ;
  tick ;
  goto #pr ;

#prRet:

  x := 66 ;
  ret := #prRet2 ;
  tick ;
  push stk ret ;
  tick ;
  goto #pr ;

#prRet2:

  halt ;

#pr:

  pop stk r ;
  print x ;
  tick ;
  goto r
