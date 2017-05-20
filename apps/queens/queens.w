-- Solve the N-queens problem
const N = 18

-- Compiler options
opt StackWidth = N

-- Program state
var poss : bit<N>  -- Possible positions of queen on current row
var l    : bit<N>  -- Squares attacked on current row due to left-diagonal
var r    : bit<N>  -- " due to right-diagonal
var d    : bit<N>  -- " due to column
var hot  : bit<N>  -- Choice of queen position on current row

poss := ~0 ;
while poss /= 0 do
  -- Isolate first hot bit in poss
  hot := poss & (~poss + 1) ;
  -- Either place a queen here or not
     ( l := (l|hot) << 1
    || r := (r|hot) >> 1
    || d := d|hot
     ; poss := ~(l|r|d) )
  ?  ( poss := poss & ~hot )
end ;
-- Fail unless every column has a queen
if d /= ~0 then fail end
