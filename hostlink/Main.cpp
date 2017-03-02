#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include "RawLink.h"

int main()
{
  RawLink link;

  char startByte;
  link.get(&startByte, 1);

  unsigned count;
  link.get(&count, 4);

  printf("%i\n", count);

  return 0;
}
