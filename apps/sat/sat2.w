-- Basic SAT Solver (Version 2)
-- Reduce number of iterations over formula

const LogMaxVars      = 9
const LogMaxLits      = 12
const LogMaxClauseLen = 5

-- Types
type VarId     = bit(LogMaxVars)
type LitId     = bit(LogMaxLits)
type ClauseLen = bit(LogMaxClauseLen+1)
enum Value     = Undef | Zero | One
rec  Lit       = { neg         : bit(1)
                 , id          : VarId
                 , next        : LitId
                 , endOfClause : bit(1)
                 , finalLit    : bit(1) }

-- Arrays
var lits : LitId -> Lit = "lits.mif"
var vars : VarId -> Value

-- Registers
var done      : bit(1)
var stop      : bit(1)
var sat       : bit(1)
var again     : bit(1)
var lit       : Lit
var branchLit : Lit
var undefLit  : Lit
var val       : Value
var i         : LitId
var size      : ClauseLen
var smallest  : ClauseLen

-- Solver
while smallest /= ~0 do
  -- Pick a literal occuring in a clause of minimum size
  i := 0 || sat := 0 || stop := 0 || again := 0 || size := 0 || smallest := ~0 ;
  while ~stop do
    lit  := lits[i] ;
    val  := vars[lit.id] ;
    i    := i+1 ||
    if (val == One) & ~lit.neg |
       (val == Zero) & lit.neg then sat := 1 end ||
    if val == Undef then undefLit := lit || size := size + 1 end ;
    if lit.endOfClause then
      if ~sat & (size < smallest) then
        if size == 0 then fail else
          if size == 1 then
            again := 1 ||
            vars[undefLit.id] := cond(undefLit.neg, Zero, One)
          else
            smallest := size || branchLit := undefLit
          end
        end
      end ;
      sat := 0 || size := 0
    end ;
    if lit.finalLit then
      if again then again := 0 || i := 0 || smallest := ~0 else stop := 1 end
    end
  end ;

  -- Make new assignment
  if smallest /= ~0 then
    val := Zero ? val := One ;
    vars[branchLit.id] := val
  end
end
