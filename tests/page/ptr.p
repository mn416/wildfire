declare

  x : nat 8,
  y : nat 8,
  p : ^(nat 8)

in

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
