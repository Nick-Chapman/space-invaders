#include <stdio.h>
#include <string.h>
#include <time.h>
#include "shared.h"
#include "SDL.h"

static int test1 ();
static int speed ();
static int play ();
static void render();
static void init_renderer();
static void fini_renderer();

int main (int argc, char* argv[]) {
  if (argc != 2) {
    printf("expected exactly one command line arg, got %d\n",argc-1);
    die
  }
  char* arg = argv[1];
  if (0 == strcmp(arg,"test1")) return test1();
  else if (0 == strcmp(arg,"speed")) return speed();
  else if (0 == strcmp(arg,"play")) return play();
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


int play () {

  printf("play...\n");
  init_renderer();

  Func fn = prog_0000;
  int frame_count = 0;
  for (;;) {
    while (fn) {
      fn = (Func)fn();
      if (icount>50000) break; //random period
    }
    frame_count++;
    //printf("calling render (%d)\n", frame_count);
    render();
  }

  fini_renderer();
  return 0;
}


static SDL_Renderer *renderer;
static SDL_Window *window;
static const int renderscale = 3;

static uint8_t *rawmem = (uint8_t*) &mem;
#include <assert.h>


static void init_renderer()
{
    int rc = SDL_Init(SDL_INIT_EVERYTHING);
    assert(rc == 0);

    window = SDL_CreateWindow("space-invaders", SDL_WINDOWPOS_CENTERED, SDL_WINDOWPOS_CENTERED,
            224 * renderscale, 256 * renderscale, 0);
    assert(window);

    SDL_ShowCursor(SDL_DISABLE);

    renderer = SDL_CreateRenderer(window, -1, SDL_RENDERER_ACCELERATED | SDL_RENDERER_PRESENTVSYNC);
    assert(renderer);
}

static void fini_renderer()
{
    SDL_DestroyRenderer(renderer);
    SDL_DestroyWindow(window);
}

static void render()
{
    SDL_SetRenderDrawColor(renderer, 0, 0, 0, 255);
    SDL_RenderClear(renderer);
    SDL_SetRenderDrawColor(renderer, 255, 255, 255, 255);

    const uint8_t *iter = rawmem + 0x2400;

    for (int y = 0; y < 224; ++y)
    {
        for (int xi = 0; xi < 32; ++xi)
        {
            uint8_t byte = *iter++;

            for (int i = 0; i < 8; ++i)
            {
                int x = xi * 8 + i;
                int on = (byte >> i) & 0x1;

                if (on)
                {
                    SDL_Rect rect;
                    rect.x = y * renderscale;
                    rect.y = (256 - x - 1) * renderscale;
                    rect.w = renderscale;
                    rect.h = renderscale;

                    SDL_RenderDrawRect(renderer, &rect);
                }
            }
        }
    }

    SDL_RenderPresent(renderer);
}


u1 e1_is_pressed(const char* s) { //TODO: use enum for buttons
  bool res = false; //(0 == strcmp(s,"coin entry"));
  //printf ("e1_is_pressed: %s --> %d\n", s, res);
  return res;
}
