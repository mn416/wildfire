declare

  s : tokens 8,
  x : nat 8,
  y : nat 8,
  z : nat 8

in

    tick
  ; (take s x || take s y)
  ; tick
  ; take s z
  ; print x
  ; print y
  ; print z
