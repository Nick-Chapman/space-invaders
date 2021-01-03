
typedef void* (*Control)(void);
typedef Control (*Func)();

void advance(int);
void at(const char*);
void instruction(const char*);

Control jump(Func); // identity!

Control prog_0000 ();
Control prog_0001 ();
Control prog_0002 ();
Control prog_0003 ();

Control prog_18D4 ();
