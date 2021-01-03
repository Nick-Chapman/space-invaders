#include <stdio.h>
#include "machine.h"

void advance(int n) {
  printf ("advance: %d\n",n);
}

void at(const char* s) {
  printf ("at: %s\n",s);
}

void instruction(const char* s) {
  printf ("instruction: %s\n",s);
}

Control jump(Func f) {
  printf ("jump: %p\n",f);
  return (Control)f;
}

Control prog_18D4 () {
  printf ("prog_18D4 -- STOP\n");
  return 0;
}

int main () {
  printf ("main()\n");
  Func fn = prog_0000;
  while (fn) {
    printf ("main(),loop: %p\n", fn);
    fn = (Func)fn();
  }
  printf ("main()--DONE\n");
  return 0;
}
