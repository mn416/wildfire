-- SAT Solver using DPLL algorithm

const LogMaxVars    = 9
const LogMaxLits    = 12
const LogStackDepth = 10

-- Types
type VarId      = bit(LogMaxVars)
type LitId      = bit(LogMaxLits)
type StackIndex = bit(LogStackDepth)
enum Value      = Undef | Zero | One
rec  Lit        = { neg         : bit(1)
                  , id          : VarId
                  , next        : LitId
                  , endOfClause : bit(1)
                  , finalLit    : bit(1) }

-- Arrays
var lits   : LitId      -> Lit    =   "lits.mif"
var vars   : VarId      -> Value
var stack  : StackIndex => Lit 

-- Registers
var done   : bit(1)
var found  : bit(1)
var lit    : Lit
var val    : Value
var i      : LitId
var j      : LitId
var k      : LitId
var sp     : StackIndex
var root   : Lit
var iter   : bit(1)
var last   : bit(1)
var sat    : bit(1)
var undef  : bit(2)
var unit   : Lit
var nextj  : LitId

-- Solver
while ~done do
  -- Find the next unassigned variable
  found := 0 ;
  while ~found & ~done do
    lit := lits[i] ;
    val := vars[lit.id] ;
    done := lit.finalLit ||
    if val == Undef then found := 1 else i := i+1 end
  end ;

  if found then
    val := Zero ? val := One ;

    -- Assign variable and push literal to stack
    vars[lit.id] := val ||
    stack[0] := lit ||
    sp := 1 ;

    while sp /= 0 do
      root := stack[sp-1] ;
      sp := sp-1 || j := root.next || iter := 1 ;

      -- Loop over each clause containing literal
      while iter do
        -- Loop over each literal in clause
        undef := 0 || k := 0 || sat := 0 || last := 0 ;
        while ~last do
          lit := lits[j+k] ;
          val := vars[lit.id] ;
          if lit.id == root.id then nextj := lit.next end ||
          if (val == One) & ~lit.neg |
             (val == Zero) & lit.neg then sat := 1 end ||
          if (val == Undef) & (undef /= 2) then
            undef := undef+1 || unit := lit end ||
          last := lit.endOfClause ||
          k := k+1
        end ;

        -- Fail if clause is unsatisfiable
        if ~sat & (undef == 0) then fail end ;

        -- Unit propagation
        if ~sat & (undef == 1) then
          vars[unit.id] := cond(unit.neg, Zero, One) ||
          stack[sp] := unit ;
          sp := sp+1
        end ;

        j := nextj || if nextj == root.next then iter := 0 end
      end
    end
  end
end
