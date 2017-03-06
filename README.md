# Wildfire

Wildfire is a hardware compiler for a simple imperative language
extended with **non-deterministic choice**.  For example, here is a
wildfire program that solves the
[N-Queens](https://en.wikipedia.org/wiki/Eight_queens_puzzle) problem:

```ada
declare
    -- Set the widths to N to solve N-Queens
    poss : 8,  -- Possible positions of queen on current row
    l    : 8,  -- Squares attacked on current row due to left-diagonal
    r    : 8,  -- " due to right-diagonal
    d    : 8,  -- " due to column
    bit  : 8   -- Choice of queen position on current row
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
```

The source language supports sequential composition (`;`), parallel
composition (`||`), loops (`while`), conditionals (`if`),
non-deterministic choice (`?`), success (`halt`), and failure
(`fail`).

The compiler works by creating *n* instances of the program (which we
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
efficient:

A. Determining an idle neighbour is a single-cycle operation.

B. Copying a processor's state to a neighbouring processor is also a
single-cycle operation.

A corollary of (A) and (B) is that spawning work is always cheaper
than executing it sequentially.  (Pushing state onto the stack is a
multicycle operation.)  This means the program scales well to large
numbers of processors.

The language provides register variables only.  Block RAM variables
(arrays) would extend the range of problems that can be expressed,
e.g. SAT.  Of course, it would then be difficult to preserve property
(B).

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
takes 548s. (It didn't parallelise well using GCC Cilk.)

(More results [here](doc).)
