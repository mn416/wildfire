# Wildfire

Wildfire is a hardware compiler for a simple imperative language
extended with **non-deterministic choice**.  For example, here is a
wildfire program that solves the
[N-Queens](https://en.wikipedia.org/wiki/Eight_queens_puzzle) problem:

```ada
-- Number of queens
const N = 8

-- Program state
var poss : bit(N)  -- Possible positions of queen on current row
var l    : bit(N)  -- Squares attacked on current row due to left-diagonal
var r    : bit(N)  -- ... due to right-diagonal
var d    : bit(N)  -- ... due to column
var hot  : bit(N)  -- Choice of queen position on current row

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
```

The source language supports sequential composition (`;`), parallel
composition (`||`), loops (`while`), conditionals (`if`),
non-deterministic choice (`?`), success (`halt`), and failure
(`fail`).

The compiler works by creating many instances of the program (which we
call *processors*) on FPGA, connected according to a topology
specified at compile-time.

To execute a statement *s1 ? s2*, a processor *p* either:

1. Copies its local state to a neighbouring processor and tells it to
execute *s2*.  Meanwhile *p* proceeds by executing *s1*.  In this
case, we say that *p* spawns *s2*.

2. If no neighbouring processor is idle then the local state is
pushed onto *p*'s stack. Processor *p* executes *s1* and then, after
backtracking, *s2*.

There are two properties of the implementation that make this
efficient: (1) determining an idle neighbour is a single-cycle
operation; (2) copying a processor's state to a neighbouring processor
is optimised.

The language supports arrays as well as register variables.  Arrays
are implemented using on-chip block RAMs.  This means they tend to be
small, and hence quick to copy.  Read-only arrays are implemented as
block RAMs that are shared between any number of processors (the
number can be changed using a compiler option) -- they can quite a bit
larger than read-write arrays and they don't need to be copied during
spawning.

So far there are three wildfire applications:

  * [N-Queens solver](apps/queens/queens.w)
  * [Golomb ruler solver](apps/golomb/golomb.w)
  * [SAT solver](apps/dpll/dpll.w)

Here are the results for an 18-Queens solver on a DE5-NET FPGA:

  Metric       | Value
  ------------ | ------------------
  Processors   | 512
  Topology     | 64x8 butterfly
  Frequency    | 200MHz
  Logic (ALMs) | 157K (67%)
  BRAMs        | 9.4Mbit (18%)
  Runtime      | 9.7s

A C++ version of the program running on a 2.6GHz Intel Core i7-6770HQ
takes 548s. (But didn't parallelise well using GCC Cilk.)

More results [here](doc/timings.md).
