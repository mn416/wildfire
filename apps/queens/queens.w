-- Compiler options
opt StackWidth = 18

-- Set the widths to N to solve N-Queens
var poss : 18  -- Possible positions of queen on current row
var l    : 18  -- Squares attacked on current row due to left-diagonal
var r    : 18  -- " due to right-diagonal
var d    : 18  -- " due to column
var bit  : 18  -- Choice of queen position on current row

poss := ~0 ;
while poss /= 0 do
  -- Isolate first hot bit in poss
  bit := poss & (~poss + 1) ;
  -- Either place a queen here or not
     ( l := (l|bit) << 1
    || r := (r|bit) >> 1
    || d := d|bit
     ; poss := ~(l|r|d) )
  ?  ( poss := poss & ~bit )
end ;
-- Fail unless every column has a queen
if d /= ~0 then fail end
