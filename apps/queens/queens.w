-- Set the widths to N to solve N-Queens
var poss : 8  -- Possible positions of queen on current row
var l    : 8  -- Squares attacked on current row due to left-diagonal
var r    : 8  -- " due to right-diagonal
var d    : 8  -- " due to column
var bit  : 8  -- Choice of queen position on current row

begin
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
end
