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

  return 0;
}

/* Data Marshalling */
int read_cmd(byte* buf)
{
  int len;
  if( (read_exact(buf, 2) != 2) ) return -1;
  len = (buf[0] << 8) | buf[1];
  return read_exact(buf, len);
}

int write_cmd(byte* buff, int size)
{
  return 0;
}

int read_exact(byte* buf, int size)
{
  int i, got = 0;

  do {
    if ( ( i = read(0, buf+got, size-got) ) <= 0 ) {
      return i;
    }
    got += i;
  } while (got < size);

  return got;
}

int write_exact(byte* buf, int size)
{
  int i, wrote = 0;

  do {
    if ( ( i = write(1, buf+wrote, size-wrote) ) <= 0 ) return i;
    wrote += i;
  } while (wrote < size);

  return wrote;
}

