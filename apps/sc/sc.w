-- Sequential consistency checker
-- Based on algorithm by P. B. Gibbons and E. Korach
-- TODO: avoided repeated states

const LogMaxAddrs   = 4
const LogMaxVals    = 13
const LogMaxOps     = 14
const LogMaxReaders = 5
const LogMaxThreads = 2
const NumThreads    = 4

type Addr           = bit(LogMaxAddrs)
type Val            = bit(LogMaxVals)
type OpId           = bit(LogMaxOps)
type NumReaders     = bit(LogMaxReaders)
type ThreadId       = bit(LogMaxThreads)

enum Tag = End | Load | Store
struct Op = { tag : Tag, addr : Addr, val : Val }

var trace    : OpId     -> Op         = "trace.mif"
var next     : ThreadId -> OpId       = "next.mif"
var readers  : Val      -> NumReaders = "readers.mif"
var mreaders : Addr     -> NumReaders = "mreaders.mif"
var mem      : Addr     -> Val

var loadMode : bit(1) = 1
var store    : bit(1) = 1
var load     : bit(1)
var t        : bit(LogMaxThreads+1)
var pc       : OpId
var op       : Op
var val      : Val
var rs       : NumReaders
var done     : bit(LogMaxThreads+1)

while store | ~loadMode do
  store := 0 || done := 0 || t := 0 ;
  while (t /= NumThreads) & (~store) do
    load := 1 ;
    while load == 1 do
      pc   := next[t] ;
      op   := trace[pc] ;
      val  := mem[op.addr] ||
      rs   := mreaders[op.addr] ||
      load := 0 ;
      if loadMode then
        if op.tag == Load then
          if val == op.val then
            mreaders[op.addr] := rs-1 ||
            next[t] := pc+1 ||
            load := 1
          end
        end
      else
        if (op.tag == Store) & (rs == 0) then
            skip
          ? (rs := readers[op.val] ;
             mreaders[op.addr] := rs ||
             mem[op.addr] := op.val ||
             next[t] := pc+1 ||
             store := 1)
        end ;
        if op.tag == End then done := done+1 end
      end
    end ;
    t := t+1
  end ;
  loadMode := ~loadMode
end ;
if done /= NumThreads then fail end
