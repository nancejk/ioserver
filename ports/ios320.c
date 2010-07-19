/* ios320.c
 * C module for controlling the IOS320 12 bit ADC from Erlang.
 * Written by Jared Nance - started on 6/30/2010.
 */

/* Standard C headers */
#include <stdio.h>
#include <unistd.h>
#include <ei.h>

/* Acromag includes */
#include <ioscarrier.h>
#include <ios320.h>

/* The maximum buffer size in bytes. */
#define BUF_SIZE 512

/* Enumeration over possible functions that can be called. */
enum operation { READBYTEPADS, CONFIGURE, 
		 AUTOZERO, AUTOCAL, READ_CHANNELS_RAW,
		 READ_CHANNELS_CAL, READ_CHANNELS_COR};

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

/* Typedef for cblock320 struct */
typedef struct cblk320 cblk320;

int main()
{
  byte buffer[BUF_SIZE];
  ei_x_buff result;

  struct scan_array s_array[SA_SIZE];
  word autozero_data[SA_SIZE];
  word calibrated_data[SA_SIZE];
  word raw_data[SA_SIZE];
  int cor_data[SA_SIZE];
  cblk320 config_parameters;
  long addr;

  /* Start the carrier and get going. */
  if( InitCarrierLib() != S_OK ) {
    fprintf(stderr, "Could not initialize carrier lib!\n");
    return (-1);
  }

  int data_size = 0;
  while( ( data_size = read_cmd(buffer) ) > 0 ) {
    /* Read the bytepads associated with the compiled cblk320 on this system*/
    if( buffer[0] == READBYTEPADS ) {
      if( ei_x_new_with_version(&result) || ei_x_encode_tuple_header(&result, 2)) return (-1);
      if( ei_x_encode_atom(&result, "ok") ) return (-1);

      cblk320_byte_pads(&buffer, 21);
      if( ei_x_encode_list_header(&result, 21) ) return (-1);
      for(int i = 0; i < 21; i++) ei_x_encode_char(&result, buffer[i]);
      ei_x_encode_empty_list(&result);
    }

    /* (Re)configure the cblk320 */
    else if( buffer[0] == CONFIGURE ) {
      if( ei_x_new_with_version(&result) || ei_x_encode_tuple_header(&result, 2)) return (-1);

      memcpy(&config_parameters, &(buffer[1]), sizeof(cblk320));
      if( ei_x_encode_atom(&result, "ok") || ei_x_encode_atom(&result, "configured") ) return (-1);
    }

    /* Run the autozero routine for the cblk320 */
    else if( buffer[0] == AUTOZERO ) {
      if( ei_x_new_with_version(&result) || ei_x_encode_tuple_header(&result, 2)) return (-1);
      if( ei_x_encode_atom(&result, "ok") || ei_x_encode_atom(&result, "autozero") ) return (-1);
    }

    /* Run the autocalibrate routine for the cblk320 */
    else if( buffer[0] == AUTOCAL ) {
      if( ei_x_new_with_version(&result) || ei_x_encode_tuple_header(&result, 2)) return (-1);
      if( ei_x_encode_atom(&result, "ok") || ei_x_encode_atom(&result, "autocalibrate") ) return (-1);
    }

    /* Read the raw ADC counts from the card */
    else if( buffer[0] == READ_CHANNELS_RAW ) {
      if( ei_x_new_with_version(&result) || ei_x_encode_tuple_header(&result, 2)) return (-1);
      if( ei_x_encode_atom(&result, "ok") || ei_x_encode_atom(&result, "read_raw") ) return (-1);
    }

    /* Read calibrated ADC counts from the card */
    else if( buffer[0] == READ_CHANNELS_CAL ) {
      if( ei_x_new_with_version(&result) || ei_x_encode_tuple_header(&result, 2)) return (-1);
      if( ei_x_encode_atom(&result, "ok") || ei_x_encode_atom(&result, "read_cal") ) return (-1);
    }

    /* Read corrected data from the card */
    else if( buffer[0] == READ_CHANNELS_COR ) {
      if( ei_x_new_with_version(&result) || ei_x_encode_tuple_header(&result, 2)) return (-1);
      if( ei_x_encode_atom(&result, "ok") || ei_x_encode_atom(&result, "read_cor") ) return (-1);
    }

    write_cmd(&result);
    ei_x_free(&result);
  }

  return 0;
}
