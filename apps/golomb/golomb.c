#include <stdio.h>
#include <stdint.h>

// Search for ruler with NumMarks and MaxLength
int NumMarks  = 14;
int MaxLength = 126;

// For midpoint reduction rule
int MidPoint = 63; // (MaxLength+1)/2;
int HalfMarks = 7; // (NumMarks+1)/2;

typedef struct {
  uint64_t w[2];
} BV;

int golomb(int marks, int len, BV ruler, BV dist)
{
  uint32_t count = 0;
  while (marks != NumMarks) {
    if ((ruler.w[1] >> 62) ||
          ((marks < HalfMarks) && (len > MidPoint))) return count;
    if ((ruler.w[0] & dist.w[0]) == 0 && (ruler.w[1] & dist.w[1]) == 0) {
      BV newRuler;
      newRuler.w[1] = (ruler.w[1] << 1) | (ruler.w[0] >> 63);
      newRuler.w[0] = (ruler.w[0] << 1);
      count += golomb(marks, len+1, newRuler, dist);

      marks++;
      dist.w[0] = dist.w[0] | ruler.w[0];
      dist.w[1] = dist.w[1] | ruler.w[1];
      ruler.w[1] = (ruler.w[1] << 1) | (ruler.w[0] >> 63);
      ruler.w[0] = (ruler.w[0] << 1) | 1;
      len++;
    }
    else {
      ruler.w[1] = (ruler.w[1] << 1) | (ruler.w[0] >> 63);
      ruler.w[0] = (ruler.w[0] << 1);
      len++;
    }
  }
  return 1+count;
}


int main()
{
  BV ruler, dist;

  ruler.w[0] = 1; ruler.w[1] = 0;
  dist.w[0] = dist.w[1] = 0;

  printf("%d\n", golomb(1, 0, ruler, dist));
}
