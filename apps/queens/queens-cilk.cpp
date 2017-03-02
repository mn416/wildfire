#include <stdio.h>

//#define _Cilk_spawn
//#define _Cilk_sync

const int N = 16;

int search(int poss, int left, int down, int right)
{
  int MASK = (1<<N) - 1;
  if (poss == 0) {
    if (down == MASK) return 1;
    else return 0;
  }
  else {
    int bit = poss & -poss;
    int n = _Cilk_spawn search(poss & ~bit, left, down, right);
    left = (left|bit)<<1;
    right = (right|bit)>>1;
    down = down|bit;
    int m = _Cilk_spawn search((~(left|down|right)) & MASK, left, down, right);
    _Cilk_sync;
    return n + m;
  }
}

int main(void)
{
  int MASK = (1<<N) - 1;
  int n = search(MASK, 0, 0, 0);
  printf("%d\n", n);
  return 0;
}
