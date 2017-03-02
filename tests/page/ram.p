declare

  m   : nat 8 :-> nat 8,
  i   : nat 8,
  acc : nat 8

in

  i := 0 ; tick ; while i < 10 (
    m[i] := 65 + i ;
    i := i + 1 ;
    tick
  ) ;

  i := 0 ; tick ; while i < 10 (
    fetch m[i] ;
    i := i + 1 ;
    tick ;
    acc := data m ;
    tick ;
    print acc
  )
