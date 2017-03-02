declare

  ret : label,
  x   : nat 8

in

  x := 65 ;
  ret := #prRet ;
  tick ;
  goto #pr ;

#prRet:

  x := 66 ;
  ret := #prRet2 ;
  tick ;
  goto #pr ;

#prRet2:

  halt ;

#pr:

  print x ;
  goto ret
