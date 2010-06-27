#include <unistd.h>
#include <sys/io.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef unsigned char byte;

int read_cmd(byte* buf, int* size);
int read_exact(byte* buf, int len);
int read_header(byte* buf, int* size);

static const int BUF_SIZE = 128;

int main() 
{
  int size = BUF_SIZE;
  byte* buf;

  FILE* fp = fopen("payload_out","w");
  if( !fp ) return -1;

  int payload_size;
  while( (payload_size = read_cmd(buf, size) > 0 ) ) 
  {
    fprintf(fp,"%d",payload_size);
    fflush(fp);
  };

  fclose(fp);
  return 0;
}

int read_header(byte* buf, int* size)
{
  int len;
  if( read_exact(buf, 2) != 2 ) return (-1);
  len = (buf[0] << 8) | buf[1];
  return len;
}

int read_cmd(byte* buf, int* size)
{
  int len;
  if( read_exact(buf, 2) != 2 ) return (-1);
  len = (buf[0] << 8) | buf[1];
  if( len > *size ) {
    byte* tmp = (byte*) realloc(buf, len);
    if( tmp == NULL ) return -1;
    else buf = tmp;
    *size = len;
  }
  return read_exact(buf, len);
}

int read_exact(byte* buf, int len)
{
  int i, got = 0;
  do {
      if ( ( i = read(0, buf+got, len-got) ) <= 0 ) return i;
      got += i;
    } while (got < len);
  return len;
}
