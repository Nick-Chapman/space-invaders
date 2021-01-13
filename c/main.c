#include <stdio.h>
#include <string.h>
#include "shared.h"
#include "SDL.h"
#include <sys/time.h>

static int test1 ();
static int speed ();
static int play ();

static void render(SDL_Renderer*);
static void input();

static u64 time() { //in micro-seconds
  struct timeval tv;
  gettimeofday(&tv,NULL);
  return tv.tv_sec*(u64)1000000+tv.tv_usec;
}

static bool use_per_address_programs = true;
static bool use_fast_programs = false;

int main (int argc, char* argv[]) {
  if (argc != 2) {
    printf("expected exactly one command line arg, got %d\n",argc-1);
    die;
  }
  char* arg = argv[1];

  if (0 == strcmp(arg,"testA")) { use_per_address_programs = false; return test1(); }
  else if (0 == strcmp(arg,"testB")) return test1();
  else if (0 == strcmp(arg,"testC")) { use_fast_programs = true; return test1(); }

  else if (0 == strcmp(arg,"speedA")) { use_per_address_programs = false; return speed(); }
  else if (0 == strcmp(arg,"speedB")) return speed();
  else if (0 == strcmp(arg,"speedC")) { use_fast_programs = true; return speed(); }

  else if (0 == strcmp(arg,"play0")) { use_per_address_programs = false; return play(); }
  else if (0 == strcmp(arg,"play")) return play();
  else {
    printf("unexpected command line arg: \"%s\"\n",arg);
    die;
  }
}

static bool dump_state_every_instruction = false;
static int icount = 0;

static void dump_state(const char* instruction, u16 pcAfterInstructionDecode) {
  printf("%8d  [%08ld] "
         "PC:%04X "
         "A:%02X B:%02X C:%02X D:%02X E:%02X HL:%02X%02X SP:%02X%02X "
         "SZAPY:%1d%1d%1d%1d%1d"
         " : %s\n",
         icount,
         cycles,
         pcAfterInstructionDecode, //odd, but matches existing traces!
         A,B,C,D,E,H,L,SPH,SPL,
         FlagS,FlagZ,FlagA,FlagP,FlagCY,
         instruction
         );
}

void f_instruction(const char* instruction, u16 pcAfterInstructionDecode) {
  if (dump_state_every_instruction) {
    dump_state(instruction,pcAfterInstructionDecode);
  }
  icount++;
}

static Func initial_program() {
  if (use_per_address_programs) {
    if (use_fast_programs) {
      return fast_0000;
    } else {
      return slow_0000;
    }
  } else {
    return (Func)jump16(0x0);
  }
}

int test1 () {
  dump_state_every_instruction = true;
  Func fn = initial_program();
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
  Func fn = initial_program();
  u64 tic = time();
  cycles = 0;
  while (fn) {
    fn = (Func)fn();
    if (cycles > TWO_MEG * sim_seconds_to_run_for) break;
  }
  u64 toc = time();
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

enum Keys {
    KEYS_LEFT      =  1,
    KEYS_RIGHT     =  2,
    KEYS_START     =  4,
    KEYS_START2    =  8,
    KEYS_FIRE      =  16,
    KEYS_COIN      =  32,
    KEYS_TILT      =  64,
    KEYS_SLOWER    =  128,
    KEYS_FASTER    =  256,
    KEYS_QUIT      =  2048
};

static u64 keystate;

#define BIT(x) (!!(keystate & (x)))

u1 e1_is_pressed(Button but) {
  switch (but) {
  case CoinEntry: { return BIT(KEYS_COIN); }
  case Tilt: { return BIT(KEYS_TILT); }
  case P1start: { return BIT(KEYS_START); }
  case P1left: { return BIT(KEYS_LEFT); }
  case P1right: { return BIT(KEYS_RIGHT); }
  case P1shoot: { return BIT(KEYS_FIRE); }
  case P2start: { return BIT(KEYS_START2); }
  case P2left: { return BIT(KEYS_LEFT); }
  case P2right: { return BIT(KEYS_RIGHT); }
  case P2shoot: { return BIT(KEYS_FIRE); }
  case Dip3_livesLow: { return 0; }
  case Dip5_livesHigh: { return 0; }
  case Dip6_extraShipEarly: { return 0; }
  case Dip7_coinInfoOff: { return 0; }
  default: { printf("e1_is_pressed: but = %d\n",but); die; }
  }
}

static const int renderscale = 3;
const long cycles_between_frames = (TWO_MEG / 60);

int play () {
  SDL_Init(SDL_INIT_EVERYTHING);
  SDL_Window* window =
    SDL_CreateWindow("space-invaders", SDL_WINDOWPOS_CENTERED, SDL_WINDOWPOS_CENTERED,
                     224 * renderscale, 256 * renderscale, 0);
  SDL_ShowCursor(SDL_DISABLE);
  SDL_Renderer* renderer =
    // Think something here causes fps to be limited to about 60 fps?
    SDL_CreateRenderer(window, -1, SDL_RENDERER_ACCELERATED | SDL_RENDERER_PRESENTVSYNC);
  Func fn = initial_program();
  long frame_count = 0;
  long frame_credit = cycles_between_frames;
  double speedup = 1.0;
  while (fn) {
    const long c1 = cycles;
    fn = (Func)fn();
    const long c2 = cycles;
    frame_credit -= (c2-c1);
    if (frame_credit < 0) {
      frame_count ++;
      frame_credit += (cycles_between_frames * speedup);
      render(renderer);
      input();
      if (BIT(KEYS_QUIT)) fn = 0;

      if (BIT(KEYS_SLOWER)) {
        speedup /= 1.1;
        printf("slower, speedup = x%g\n",speedup);
      }
      if (BIT(KEYS_FASTER)) {
        speedup *= 1.1;
        printf("faster, speedup = x%g\n",speedup);
      }
    }
  }
  SDL_DestroyRenderer(renderer);
  SDL_DestroyWindow(window);
  return 0;
}

static void measure_fps() {
  static int frames = 0;
  static u64 last = 0;
  if (last == 0) {
    last = time();
  } else {
    frames++;
    u64 now = time();
    u64 elapsed = now - last;
    if (elapsed >= MEG) {
      printf("fps = %d\n",frames);
      frames = 0;
      last = now;
    }
  }
}

static void render(SDL_Renderer* renderer) {
  measure_fps();
  SDL_SetRenderDrawColor(renderer, 10, 10, 10, 255);
  SDL_RenderClear(renderer);
  SDL_SetRenderDrawColor(renderer, 255, 255, 255, 255);
  const u8 *iter = mem + 0x2400;
  for (int y = 0; y < 224; ++y) {
    for (int xi = 0; xi < 32; ++xi) {
      u8 byte = *iter++;
      for (int i = 0; i < 8; ++i) {
        int x = xi * 8 + i;
        int on = (byte >> i) & 0x1;
        if (on) {
          SDL_Rect rect;
          rect.x = y * renderscale;
          rect.y = (256 - x - 1) * renderscale;
          rect.w = renderscale;
          rect.h = renderscale;
          SDL_RenderFillRect(renderer, &rect);
        }
      }
    }
  }
  SDL_RenderPresent(renderer);
}

static void input() {
  SDL_Event event_buffer[64];
  size_t num = 0;
  while (num < 64) {
    int has = SDL_PollEvent(&event_buffer[num]);
    if (!has) break;
    num++;
  }
  for (size_t i = 0; i < num; ++i) {
    SDL_Event e = event_buffer[i];
    if (e.type == SDL_QUIT) {
      e.type = SDL_KEYDOWN;
      e.key.keysym.sym = SDLK_ESCAPE;
    }
    if (! (e.type == SDL_KEYDOWN || e.type == SDL_KEYUP)) continue;
    u64 mask = 0;
    u64 f = e.type == SDL_KEYDOWN;
    switch (e.key.keysym.sym) {
#define KEY_MAP(x, y) case x: mask = y; break;
      KEY_MAP('z', KEYS_LEFT);
      KEY_MAP('x', KEYS_RIGHT);
      KEY_MAP(SDLK_F1, KEYS_START);
      KEY_MAP(SDLK_F2, KEYS_START2);
      KEY_MAP(SDLK_RETURN, KEYS_FIRE);
      KEY_MAP(SDLK_INSERT, KEYS_COIN);
      KEY_MAP(SDLK_TAB, KEYS_TILT);
      KEY_MAP(',', KEYS_SLOWER);
      KEY_MAP('.', KEYS_FASTER);
      KEY_MAP(SDLK_ESCAPE, KEYS_QUIT);
    }
    keystate = (keystate & ~mask) | (-f & mask);
  }
}



Control jump16(u16 pc) {

  if (use_per_address_programs) {

    if (pc>=ROM_SIZE) {
      printf ("jump16: (a>=ROM_SIZE) : a=%04x, ROM_SIZE=%04x\n",pc,ROM_SIZE);
      die;
    }
    Func fn =
      use_fast_programs
      ? fast_progs_array[pc]
      : slow_progs_array[pc];

    if (fn == 0) {
      printf ("jump16: no program for target address: %04x\n",pc);
      die;
    }
    return jumpDirect(pc,fn);
  }

  u8 byte = mem[pc];

  if (byte==0xD3) { //OUT
    u8 imm1 = mem[pc+1];
    Func fn = output_instruction_array[imm1];
    u16 pcAfterDecode = pc+2;
    PCH = pcAfterDecode >> 8;
    PCL = pcAfterDecode & 0xFF;
    return jumpDirect(pc,fn);

  } else if (byte==0xDB) { //IN

    u8 imm1 = mem[pc+1];
    Func fn = input_instruction_array[imm1];
    u16 pcAfterDecode = pc+2;
    PCH = pcAfterDecode >> 8;
    PCL = pcAfterDecode & 0xFF;
    return jumpDirect(pc,fn);

  } else { //other op-code

    Func fn = ops_array[byte];
    u16 pcAfterDecode = pc+1;
    PCH = pcAfterDecode >> 8;
    PCL = pcAfterDecode & 0xFF;
    return jumpDirect(pc,fn);
  }

}
