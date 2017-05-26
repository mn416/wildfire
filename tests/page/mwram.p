var m   : ram <8->2> <6->8>
var i   : reg 8
var acc : reg 8

m:A[0] := 1 ; tick ;
m:A[1] := 0 ; tick ;
m:A[2] := 0 ; tick ;
m:A[3] := 1 ; tick ;
fetch m:B[0] ; tick ;
acc := data(m:B) ; tick ;
print acc
