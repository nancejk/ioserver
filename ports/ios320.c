/* ios320.c
 * C module for controlling the IOS320 12 bit ADC from Erlang.
 * Written by Jared Nance - started on 6/30/2010.
 */

typedef unsigned char byte;

/* Standard C headers */
#include <stdio.h>
#include <unistd.h>
#include <ei.h>

/* The maximum buffer size in bytes. */
#define BUF_SIZE 512

/* Enumeration over possible functions that can be called. */
enum operation { READBYTEPADS };

/* Macros for calculating struct offsets and field sizes.  offsetof(type,field)
 * could be included from stddef.h, but is instead replicated
 * here for documentation purposes.
 */
#define offsetof(type, field) ((size_t)(&((type *)0)->field))
#define fieldsize(type, field) ((size_t)sizeof(((type*)0)->field))

/* Forward declarations of the erl_comm functions defined
 * in erl_comm.c. 
 */
int read_cmd(byte* buffer);
int read_exact(byte* buffer, int len);
int write_cmd(ei_x_buff* x);
int write_exact(byte* buffer, int len);

int main()
{
  byte buffer[BUF_SIZE];
  ei_x_buff result;

  int data_size = 0;
  while( ( data_size = read_cmd(buffer) ) > 0 ) {
    if( buffer[0] == READBYTEPADS ) {
      if( ei_x_new_with_version(&result) || ei_x_encode_tuple_header(&result, 2)) return (-1);
      if( ei_x_encode_atom(&result, "ok") || ei_x_encode_atom(&result, "bytepads") ) return (-2);
    }
    write_cmd(&result);
    ei_x_free(&result);
  }

  return 0;
}
