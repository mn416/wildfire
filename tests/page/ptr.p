var x : reg 8
var y : reg 8
var p : ptr 8

x := 65 ;
y := 66 ;
tick ;
p := ^y ;
tick ;
^p := 67 ;
tick ;
print x ;
tick ;
print y
