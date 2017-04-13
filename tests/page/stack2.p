var stk : ram 8 8
var x   : reg 8
var y   : reg 8

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
