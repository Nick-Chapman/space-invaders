#include <stdio.h>
#include "machine.h"
Control prog_0000 ()
{
    at ( "0000" );
    instruction ( "NOP" );
    advance ( 4 );
    return jump ( prog_0001 );
}

Control prog_0001 ()
{
    at ( "0001" );
    instruction ( "NOP" );
    advance ( 4 );
    return jump ( prog_0002 );
}

Control prog_0002 ()
{
    at ( "0002" );
    instruction ( "NOP" );
    advance ( 4 );
    return jump ( prog_0003 );
}

Control prog_0003 ()
{
    at ( "0003" );
    instruction ( "JP   18D4" );
    advance ( 10 );
    return jump ( prog_18D4 );
}

