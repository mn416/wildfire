var stk : ram 8 8
var x   : reg 8
var y   : reg 12

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
