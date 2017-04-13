var ret : label
var r   : label
var stk : ram 8 8
var x   : reg 8

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
