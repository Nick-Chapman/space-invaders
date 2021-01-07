
#include <stdlib.h>
#include <stdbool.h>

#define die { printf("die: %s:%d (%s)\n", __FILE__, __LINE__, __FUNCTION__); exit(1); }

typedef void* (*Control)(void);
typedef Control (*Func)();

extern Control prog_0000 ();
extern int icount;
extern long cycles;
extern bool dump_state_every_instruction;

typedef bool u1;
typedef unsigned char u8; // TODO: use uint8_t etc
typedef unsigned short u16;
typedef unsigned int u17;

#define MEM_SIZE 0x4000

extern u8 mem [MEM_SIZE];
