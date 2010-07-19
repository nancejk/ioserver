#include <unistd.h>
#include <stdio.h>
#include <ei.h>

typedef unsigned char byte;

/* Erlang sends its binaries prefixed with a 
 * two byte length header.  The effect of this
 * function is to read the length header and from
 * that information, populate the buffer from 
 * stdin.
 */
int read_cmd(byte* buffer);

/* This reads no more than len bytes from stdin
 * and writes them to the buffer.
 */
int read_exact(byte* buffer, int len);

/* TODO: document */
int write_cmd(ei_x_buff* x);
int write_exact(char* buffer, int len);


int read_cmd(byte* buffer)
{
  /* This int will store the data length */
  int len;

  if ( read_exact(buffer, 2) != 2 ) return (-1);
  len = ( buffer[0] << 8 ) | buffer[1];

  return read_exact(buffer, len);
}

int read_exact(byte* buffer, int len)
{
  int i, got=0;
  do {
    if( ( i = read(0, buffer+got, len-got) ) <= 0 )
      return i;
    got += i;
  } while ( got < len );

  return got;
}

int write_cmd(ei_x_buff *buff)
{
  char li;

  li = ( buff->index >> 8 ) & 0xff;
  write_exact(&li, 1);

  li = buff->index & 0xff;
  write_exact(&li, 1);
  return write_exact(buff->buff, buff->index);
}

int write_exact(char* buffer, int len)
{
  int i, wrote = 0;

  do {
    if ( ( i = write(1, buffer+wrote, len-wrote) ) <= 0 ) return i;
    wrote += i;
  } while (wrote < len);

  return wrote;
}
