var ret : label
var x   : reg 8

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
