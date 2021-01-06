
#include <stdlib.h>
#include <stdbool.h>

#define die { printf("die: %s:%d (%s)\n", __FILE__, __LINE__, __FUNCTION__); exit(1); }

typedef void* (*Control)(void);
typedef Control (*Func)();

extern Control prog_0000 ();
extern int icount;
extern long cycles;
extern bool dump_state_every_instruction;
