#include <unistd.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define BUF_SIZE 128

typedef unsigned char byte;

int read_cmd(byte* buf);
int write_cmd(byte* buf, int size);
int read_exact(byte* buf, int size);
int write_exact(byte* buf, int size);

/* API */
long add(long a, long b) 
{
  return a+b;
}

long multiply(long a, long b)
{
  return a*b;
}

double divide(double a, double b)
{
  return a/b;
}

/* MAIN */
int main()
{
  int fn, arg1, arg2, result;
  byte buff[ BUF_SIZE ];

  while( read_cmd(buff) > 0 ) {
    fn = buff[0];

    if( fn == 1 ) {
      arg1 = buff[1];
      arg2 = buff[2];
      result = add(arg1, arg2);
    }

    else if ( fn == 2 ) {
      arg1 = buff[1];
      arg2 = buff[2];

      result = multiply( arg1, arg2 );
    }

    else if ( fn == 3 ) {
      arg1 = buff[1];
      arg2 = buff[2];
      
      result = divide( arg1, arg2 );
    }

    buff[0] = result;
    write_cmd(buff, 1);
  }

  return 0;
}

