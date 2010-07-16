/* ios320.c
 * C module for controlling the IOS320 12 bit ADC from Erlang.
 * Written by Jared Nance - started on 6/30/2010.
 */

/* Acromag headers */
#include <ioscarrier/ioscarrier.h>
#include <ios320/ios320.h>

/* Standard C headers */
#include <stdio.h>
#include <unistd.h>

/* The maximum buffer size in bytes. */
#define BUF_SIZE 512

/* Enumeration over possible functions that can be called. */
enum operation { READBYTEPADS, READ_STATUS, CONFIGURE_DEFAULT, CONFIGURE, READ_CHANNELS };

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
int write_cmd(byte* buffer, int len);
int write_exact(byte* buffer, int len);

/* This avoids a lot of 'structs' in the code. */
typedef struct cblk320 cblk320;

/* A function which calculates the byte pads for each field in the cblk320 
 * struct and writes them to a buffer.  This information is necessary for
 * the (de)serialization process.
 */
int cblk320_byte_pads(byte* buf, int len)
{
  /* cblk320 has exactly 21 fields. */
  if( len != 21 ) return (-1);

  /* This is a helpful number to keep around. */
  int accumulated_length = 0;

  /* Lots of ints for calculating byte pad sizes */
  byte brd_ptr_pad, range_pad, trigger_pad, mode_pad, average_pad;
  byte gain_pad, data_mask_pad, bit_constant_pad, s_raw_buf_pad;
  byte s_az_buf_pad, s_cal_buf_pad, s_cor_buf_pad, sa_start_pad;
  byte sa_end_pad, slotLetter_pad, nHandle_pad, bCarrier_pad;
  byte bInitialized_pad, control_pad, id_prom_pad, channel_pad; 

  /* brd_ptr_pad is at the beginning of the struct. */
  brd_ptr_pad = offsetof(cblk320, range) - fieldsize(cblk320, brd_ptr);
  accumulated_length += fieldsize(cblk320, brd_ptr) + brd_ptr_pad;
  buf[0] = brd_ptr_pad;
  
  /* range is next */
  range_pad = offsetof( cblk320, trigger ) - 
    ( fieldsize(cblk320, range) + accumulated_length );
  accumulated_length += fieldsize(cblk320, range) + range_pad;
  buf[1] = range_pad;

  /* trigger is next */
  trigger_pad = offsetof( cblk320, mode ) -
    ( fieldsize(cblk320, trigger) + accumulated_length );
  accumulated_length += fieldsize(cblk320, trigger) + trigger_pad;
  buf[2] = trigger_pad;

  /* mode is next */
  mode_pad = offsetof( cblk320, average ) -
    ( fieldsize(cblk320, mode) + accumulated_length );
  accumulated_length += fieldsize(cblk320, mode) + mode_pad;
  buf[3] = mode_pad;

  /* average is next */
  average_pad = offsetof( cblk320, channel ) -
    ( fieldsize(cblk320, average) + accumulated_length );
  accumulated_length += fieldsize(cblk320, average) + average_pad;
  buf[4] = average_pad;

  /* channel is next */
  channel_pad = offsetof( cblk320, gain ) -
    ( fieldsize(cblk320, channel) + accumulated_length );
  accumulated_length += fieldsize(cblk320, channel) + channel_pad;
  buf[5] = channel_pad;

  /* gain is next */
  gain_pad = offsetof( cblk320, data_mask ) -
    ( fieldsize(cblk320, gain) + accumulated_length );
  accumulated_length += fieldsize(cblk320, gain) + gain_pad;
  buf[6] = gain_pad;

  /* data_mask is next */
  data_mask_pad = offsetof( cblk320, bit_constant ) -
    ( fieldsize(cblk320, data_mask) + accumulated_length );
  accumulated_length += fieldsize(cblk320, data_mask) + data_mask_pad;
  buf[7] = data_mask_pad;

  /* bit_constant is next */
  bit_constant_pad = offsetof( cblk320, s_raw_buf ) -
    ( fieldsize(cblk320, bit_constant) + accumulated_length );
  accumulated_length += fieldsize(cblk320, bit_constant) + bit_constant_pad;
  buf[8] = bit_constant_pad;

  /* s_raw_buf is next */
  s_raw_buf_pad = offsetof( cblk320, s_az_buf ) -
    ( fieldsize(cblk320, s_raw_buf) + accumulated_length );
  accumulated_length += fieldsize(cblk320, s_raw_buf) + s_raw_buf_pad;
  buf[9] = s_raw_buf_pad;

  /* s_az_buf is next */
  s_az_buf_pad = offsetof( cblk320, s_cal_buf ) -
    ( fieldsize(cblk320, s_az_buf) + accumulated_length );
  accumulated_length += fieldsize(cblk320, s_az_buf) + s_az_buf_pad;
  buf[10] = s_az_buf_pad;

  /* s_cal_buf is next */
  s_cal_buf_pad = offsetof( cblk320, s_cor_buf ) -
    ( fieldsize(cblk320, s_cal_buf) + accumulated_length );
  accumulated_length += fieldsize(cblk320, s_cal_buf) + s_cal_buf_pad;
  buf[11] = s_cal_buf_pad;

  /* s_cor_buf is next */
  s_cor_buf_pad = offsetof( cblk320, sa_start ) -
    ( fieldsize(cblk320, s_cor_buf) + accumulated_length );
  accumulated_length += fieldsize(cblk320, s_cor_buf) + s_cor_buf_pad;
  buf[12] = s_cor_buf_pad;

  /* sa_start is next */
  sa_start_pad = offsetof( cblk320, sa_end ) -
    ( fieldsize(cblk320, sa_start) + accumulated_length );
  accumulated_length += fieldsize(cblk320, sa_start) + sa_start_pad;
  buf[13] = sa_start_pad;

  /* sa_end is next */
  sa_end_pad = offsetof( cblk320, slotLetter ) -
    ( fieldsize(cblk320, sa_end) + accumulated_length );
  accumulated_length += fieldsize(cblk320, sa_end) + sa_end_pad;
  buf[14] = sa_end_pad;

  /* slotLetter is next */
  slotLetter_pad = offsetof( cblk320, nHandle ) -
    ( fieldsize(cblk320, slotLetter) + accumulated_length );
  accumulated_length += fieldsize(cblk320, slotLetter) + slotLetter_pad;
  buf[15] = slotLetter_pad;

  /* nHandle is next */
  nHandle_pad = offsetof( cblk320, bCarrier ) -
    ( fieldsize(cblk320, nHandle) + accumulated_length );
  accumulated_length += fieldsize(cblk320, nHandle) + nHandle_pad;
  buf[16] = nHandle_pad;

  /* bCarrier is next */
  bCarrier_pad = offsetof( cblk320, bInitialized ) -
    ( fieldsize(cblk320, bCarrier) + accumulated_length );
  accumulated_length += fieldsize(cblk320, bCarrier) + bCarrier_pad;
  buf[17] = bCarrier_pad;

  /* bInitialized is next */
  bInitialized_pad = offsetof( cblk320, control ) -
    ( fieldsize(cblk320, bInitialized) + accumulated_length );
  accumulated_length += fieldsize(cblk320, bInitialized) + bInitialized_pad;
  buf[18] = bInitialized_pad;
 
  /* control is next */
  control_pad = offsetof( cblk320, id_prom ) -
    ( fieldsize(cblk320, control) + accumulated_length );
  accumulated_length += fieldsize(cblk320, control) + control_pad;
  buf[19] = control_pad;

  /* id_prom is next */
  id_prom_pad = sizeof(cblk320) -
    ( fieldsize(cblk320, id_prom) + accumulated_length );
  accumulated_length += fieldsize(cblk320, id_prom) + id_prom_pad;
  buf[20] = id_prom_pad;

  return accumulated_length; 
}

int main()
{
  byte buffer[BUF_SIZE];

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
    if( buffer[0] == READBYTEPADS ) {
      if( cblk320_byte_pads(buffer, 21) != sizeof(cblk320) ) return (-1);
      write_cmd(buffer, 21);
    }

    else if( buffer[0] == READ_STATUS ) {
      if( config_parameters.bInitialized != 1 ) {
	buffer[0] = (-1);
	write_cmd(buffer, 1);
      }

      else {
	rsts320(&config_parameters);
	buffer[0] = 1;
	write_cmd(buffer, 1);
      }
    }

    else if( buffer[0] == CONFIGURE_DEFAULT ) {
      memcpy(&config_parameters, &(buffer[1]), sizeof(cblk320));

      config_parameters.s_raw_buf  = &raw_data[0];
      config_parameters.s_az_buf   = &autozero_data[0];
      config_parameters.s_cal_buf  = &calibrated_data[0];
      config_parameters.s_cor_buf  = &cor_data[0];
      config_parameters.sa_start   = &s_array[0];
      config_parameters.sa_end     = config_parameters.sa_start;

      if( CarrierOpen(0, &config_parameters.nHandle) != S_OK ) {
	fprintf(stderr, "Could not open carrier!\n");
      }

      if( GetCarrierAddress(config_parameters.nHandle, &addr) != S_OK ) {
	fprintf(stderr, "Could not get carrier address!\n");
      }

      if( CarrierInitialize(config_parameters.nHandle) != S_OK ) {
	fprintf(stderr, "Carrier initialization failed.\n");
      }
      else {
	config_parameters.bCarrier = TRUE;
      }

      if( GetIOSAddress(config_parameters.nHandle, config_parameters.slotLetter, &addr) != S_OK ) {
	fprintf(stderr, "Could not initialize board!");
      }
      else {
	config_parameters.brd_ptr = (struct map320 *)addr;
	config_parameters.bInitialized = TRUE;
      }
      
      buffer[0] = 0;
      write_cmd(buffer, 1);
    }

    else if( buffer[0] == READ_CHANNELS ) {
      if( config_parameters.bInitialized == FALSE ) {
	buffer[0] = 128;
	write_cmd(buffer, 1);
      }
      else {
	ainmc320(&config_parameters);
	memcpy(&buffer, &raw_data, SA_SIZE*sizeof(word));
	write_cmd(buffer, SA_SIZE*sizeof(word));
      }
    }
  }

  return 0;
}
