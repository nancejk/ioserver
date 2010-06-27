#include <ei.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/io.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define BUF_SIZE 128

typedef unsigned char byte;

int read_cmd(byte** buf, int* size);
int write_cmd(ei_x_buff* x);
int read_exact(byte* buf, int len);
int write_exact(byte* buf, int len);

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
  byte* buf;
  int size = BUF_SIZE;
  char command[MAXATOMLEN];
  int index, version, arity;
  long a, b, c;
  double x, y, z;
  ei_x_buff result;

  if ( (buf = (byte*) malloc(size)) == NULL ) return -1;

  // DEBUG
  while ( read_cmd(&buf, &size) > 0 ) {
    /* Reset the index, so that ei functions can decode terms
     * from the beginning of the buffer */
    index = 0;

    /* Strip the version byte to ensure this is the binary term */
    if ( ei_decode_version(buf, &index, &version) ) return 1;

    /* Expecting the tuple {Command, Arg1, Arg2} */
    if ( ei_decode_tuple_header(buf, &index, &arity) ) return 2;

    if ( arity != 3 ) return 3;

    if ( ei_decode_atom(buf, &index, command) ) return 4;

    /* Prepare the output buffer to hold {ok, Result} or {error, Reason}
     */
    if ( ei_x_new_with_version(&result) || ei_x_encode_tuple_header(&result, 2) ) return 5;
    if ( !strcmp("add", command) || !strcmp("multiply", command) ) {
      if ( ei_decode_long(buf, &index, &a) ) return 6;
      if ( ei_decode_long(buf, &index, &b) ) return 7;

      if ( !strcmp("add",command) ) {
	c = add(a, b);
      }
      else
	c = multiply(a, b);

      if ( ei_x_encode_atom(&result, "ok") || ei_x_encode_long(&result, c) ) return 8;
    }
    else if ( !strcmp("divide", command) ) {
      if ( !ei_decode_long(buf, &index, &a) ) x = a;
      else if ( ei_decode_double(buf, &index, &x) ) return 6;
      if ( !ei_decode_long(buf, &index, &b) ) y = b;
      else if ( ei_decode_double(buf, &index, &y) ) return 7;
      if ( y == 0.0 ) {
	if ( ei_x_encode_atom(&result, "error") || ei_x_encode_atom(&result, "division_by_zero") ) return 8;
      }
      else {
	z = divide(x,y);
	if ( ei_x_encode_atom(&result, "ok") || ei_x_encode_double(&result, z) ) return 8;
      }
    }

    else {
      if ( ei_x_encode_atom(&result, "error") || ei_x_encode_atom(&result, "unsupported_command") ) return 9;
    }

    write_cmd(&result);
    ei_x_free(&result);
  }

  return 0;
}

/* Data Marshalling */
int read_cmd(byte** buf, int* size)
{
  int len, firstchar, secondchar;
  if( (read_exact(*buf, 2) != 2) ) return -1;
  len = (*buf[0] << 8) | *buf[1];
  if( len > *size ) {
    byte* tmp = (byte*) realloc(*buf, len);
    if( tmp == NULL ) return -1;
    else *buf = tmp;
    *size = len;
  }
  return read_exact(*buf, len);
}

int write_cmd(ei_x_buff* buff)
{
  byte li;

  li = (buff->index >> 8) & 0xff;
  write_exact(&li, 1);
  li = buff->index & 0xff;
  write_exact(&li, 1);

  return write_exact(buff->buff, buff->index);
}

int read_exact(byte* buf, int len)
{
  int i, got = 0;

  do {
    if ( ( i = read(0, buf+got, len-got) ) <= 0 ) {
      return i;
    }
    got += i;
  } while (got < len);

  return len;
}

int write_exact(byte* buf, int len)
{
  int i, wrote = 0;

  do {
    if ( ( i = write(1, buf+wrote, len-wrote) ) <= 0 ) return i;
    wrote += i;
  } while (wrote < len);

  return len;
}

