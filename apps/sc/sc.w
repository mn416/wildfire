-- Sequential consistency checker
-- Based on algorithm by P. B. Gibbons and E. Korach

type Addr       = bit(LogMaxAddrs)
type Val        = bit(LogMaxVals)
type OpId       = bit(LogMaxOps)
type NumReaders = bit(LogMaxReaders)
type ThreadId   = bit(LogNumThreads)

type Cmd = Load | Store | End
type Op  = { cmd : Cmd, addr : Addr, val : Val }

var trace    : OpId     -> Cmd        = "trace.mif"
var next     : ThreadId -> OpId       = "next.mif"
var readers  : Val      -> NumReaders = "readers.mif"
var mreaders : Addr     -> NumReaders = "mreaders.mif"
var mem      : Addr     -> Val


var store : bit(1)
var load  : bit(1)
var t     : bit(LogNumThreads+1)
var pc    : OpId
var val   : Val
var rs    : NumReaders
var ops   : OpId

store := 1 ;
while store == 1 do
  store := 0 || t := 0 ;
  while t < NumThreads do
    load := 1 ;
    while load == 1 do
      pc   := next[t] ;
      op   := trace[pc] ;
      val  := mem[op.addr] ||
      rs   := mreaders[op.addr] ||
      load := 0 ;
      if op.cmd == Load then
        if val == op.val then
          mreaders[op.addr] := rs-1 ||
          next[t] := pc+1 ||
          load := 1 ||
          ops := ops+1
        end
      else
        if (op.cmd == Store) & (rs == 0) then
            skip
          ? (rs := readers[op.val] ;
             mreaders[op.addr] := rs ||
             mem[op.addr] := op.val ||
             next[t] := pc+1 ||
             store := 1 ||
             ops := ops+1)
        end
      end ;
      t := t+1
    end
  end
end ;
if ops /= TotalOps then fail end
