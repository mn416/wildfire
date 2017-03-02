#include <stdio.h>

int N, MASK, COUNT;

void search(int poss, int left, int down, int right)
{
  if (poss == 0) {
    if (down == MASK) COUNT++;
  }
  else {
    int bit = poss & -poss;
    search(poss & ~bit, left, down, right);
    left = (left|bit)<<1;
    right = (right|bit)>>1;
    down = down|bit;
    search((~(left|down|right)) & MASK, left, down, right);
  }
}

int main(void)
{
  N = 16;
  MASK = (1<<N) - 1;
  COUNT = 0;
  search(MASK, 0, 0, 0);
  printf("N=%d: %d\n", N, COUNT);
  return 0;
}
