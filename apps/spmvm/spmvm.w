-- Sparse Matrix Vector Multiplication

type VecIndex = bit(LogVecLen)
type RowIndex = bit(LogNumRows)
type MatIndex = bit(LogMatElems)
type Value    = bit(LogMaxValue)

struct Row    = { start : MatIndex, len : RowLen }
struct Assoc  = { key : VecIndex, val : Value }
struct Output = { row : RowIndex, val : Value }

var vec : VecIndex -> Value
var row : RowIndex -> MatIndex
var mat : MatIndex -> Assoc

while low+1 /= high do
  mid := (low+high)>>1 ;
  (high := mid) ? (low := mid)
end ;

r := row[low] ;
i := r.start || acc := 0 ;
while i /= r.len do
  assoc := mat[i] ;
  val := vec[assoc.key] ;
  acc := acc + val * assoc.val ||
  i := i + 1
end ;
emit(Output{row : low, val : acc})
