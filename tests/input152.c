#include <stdio.h>

asm("nop\n");

int main() {
  int x= 65535, z;
  char y;
  char *str;
  int c;
  int d;
  printf("aaa" "bbb");
  asm("ld ra, 0(fp)\n");
  asm("ret\n");
  x = (d+c)+1;
  d =d + 1;
  z = (d+c);
  y= (char )x; printf("0x%x\n", y);
  str= (char *)0; printf("0x%lx\n", (long)str);
  return(0);
}
