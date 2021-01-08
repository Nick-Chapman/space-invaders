#include <stdlib.h>
#include <stdbool.h>
#include <stdint.h>

#define die { printf("die: %s:%d (%s)\n", __FILE__, __LINE__, __FUNCTION__); exit(1); }

typedef void* (*Control)(void);
typedef Control (*Func)();

extern Control prog_0000 ();
extern int icount;
extern long cycles;
extern bool dump_state_every_instruction;

typedef bool u1;
typedef uint8_t u8;
typedef uint16_t u16;
typedef uint32_t u17;
typedef uint64_t u64;

#define MEM_SIZE 0x4000

extern u8 mem [MEM_SIZE];
