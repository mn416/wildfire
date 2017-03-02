declare

  n : nat 8,
  x : nat 8

in

  n := 11 ;
  x := 0 ;
  tick ;

  #begin:

    if n == 0 then goto #end else skip ;
    x := x + n ;
    n := n - 1 ;
    tick ;
    goto #begin ;

  #end:

  print x
