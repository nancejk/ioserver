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

typedef struct
{
  int a;
  int b;
} test_struct;

/* MAIN */
int main()
{
  byte buff[BUF_SIZE];

  test_struct* the_test;

  int data_size = 0;
  while( ( data_size = read_cmd(buff) ) > 0 ) {
    the_test = (test_struct*)buff;
    buff[0] = the_test->a;
    buff[1] = the_test->b;
    write_cmd(buff,2);
  } 
  
  return 0;
}

