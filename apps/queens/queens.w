declare

  poss : 18,
  l    : 18,
  r    : 18,
  d    : 18,
  bit  : 18

in

  poss := ~0 ;
  while poss /= 0 do
    bit := poss & (~poss + 1) ;
       ( l := (l|bit) << 1
      || r := (r|bit) >> 1
      || d := d|bit
       ; poss := ~(l|r|d) )
    ?  ( poss := poss & ~bit )
  end ;
  if d == ~0 then halt else fail end
