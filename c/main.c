#include <stdio.h>
#include <string.h>
#include <time.h>

#include "shared.h"

int test1 ();
int speed ();

int main (int argc, char* argv[]) {
  if (argc != 2) {
    printf("expected exactly one command line arg, got %d\n",argc-1);
    die
  }
  char* arg = argv[1];
  if (0 == strcmp(arg,"test1")) return test1();
  else if (0 == strcmp(arg,"speed")) return speed();
  else {
    printf("unexpected command line arg: \"%s\"\n",arg);
    die
  }
}

int test1 () {
  dump_state_every_instruction = true;
  Func fn = prog_0000;
  while (fn) {
    fn = (Func)fn();
    if (icount>50000) break;
  }
  printf("STOP\n");
  return 0;
}

#define MEG 1000000
#define TWO_MEG 2000000

int speed () {
  dump_state_every_instruction = false;
  const int sim_seconds_to_run_for = 60;
  Func fn = prog_0000;
  clock_t tic = clock();
  cycles = 0;
  while (fn) {
    fn = (Func)fn();
    if (cycles > TWO_MEG * sim_seconds_to_run_for) break;
  }
  clock_t toc = clock();
  clock_t duration_us = toc - tic;
  double duration_s = duration_us / (double)MEG;
  int mhz = cycles/duration_us;
  int secs = cycles/TWO_MEG;
  int speedup = mhz/2;
  printf("sim-time(secs)=%d, "
         "cycles=%ld, "
         "duration(us)=%ld, "
         "duration(s)=%.3g, "
         "mhz=%d, "
         "speedup=x%d"
         "\n",
         secs,
         cycles,
         duration_us,
         duration_s,
         mhz,
         speedup
         );
  return 0;
}
