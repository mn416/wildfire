var m   : ram 8 8
var i   : reg 8
var acc : reg 8

i := 0 ; tick ; while i < 10 do
  m[i] := 65 + i ;
  i := i + 1 ;
  tick
end ;

i := 0 ; tick ; while i < 10 do
  fetch m[i] ;
  i := i + 1 ;
  tick ;
  acc := data m ;
  tick ;
  print acc
end
