var r   : rom 8 8 := "init.mif"
var i   : reg 8   := 0
var acc : reg 8   := 0
var x   : reg 8

while i < 10 do
  load x r:a[i] ;
  tick ;
  acc := acc + x ;
  i := i+1 ;
  tick
end ;

print acc
