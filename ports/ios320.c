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
#define BUF_SIZE 1024

/* Enumeration over possible functions that can be called. */
enum operation { READBYTEPADS, CONFIGURE, 
		 AUTOZERO, AUTOCAL, READ_CHANNELS_RAW,
		 READ_CHANNELS_COR, INITIALIZE=99};

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

int cblk320_byte_pads(byte* buffer, int size);

/* Typedef for cblock320 struct */
typedef struct cblk320 cblk320;

int main()
{
  byte buffer[BUF_SIZE];
  ei_x_buff result;

  struct scan_array s_array_20ch[SA_SIZE];
  struct scan_array s_array_40ch[2*SA_SIZE];

  word autozero_data_20ch[SA_SIZE];
  word autozero_data_40ch[2*SA_SIZE];

  word calibrated_data_20ch[SA_SIZE];
  word calibrated_data_40ch[2*SA_SIZE];

  word raw_data_20ch[SA_SIZE];
  word raw_data_40ch[2*SA_SIZE];

  int cor_data_20ch[SA_SIZE];
  int cor_data_40ch[SA_SIZE];

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
      int legitimate_config = 1;
      if( ei_x_new_with_version(&result) || ei_x_encode_tuple_header(&result, 2)) return (-1);

      memcpy(&config_parameters, &(buffer[1]), sizeof(cblk320));

      /* Use the mode parameter to appropriately set the scan arrays.  In Differential mode there
      *  are 20 channels to be read. */
      if( config_parameters.mode == DIF ) {
	config_parameters.s_raw_buf = raw_data_20ch;
	config_parameters.s_az_buf = autozero_data_20ch;
	config_parameters.s_cal_buf = calibrated_data_20ch;
	config_parameters.s_cor_buf = cor_data_20ch;
	config_parameters.sa_start  = s_array_20ch;
	config_parameters.sa_end    = &(s_array_20ch[SA_SIZE-1]);
	for( int i = 0; i < SA_SIZE; i ++ ) {
	  s_array_20ch[i].chn = i;
	  s_array_20ch[i].gain = config_parameters.gain;
	}
      }
      /* Single ended mode has 40 channels */
      else if( config_parameters.mode == SEI ) {
	config_parameters.s_raw_buf = raw_data_40ch;
	config_parameters.s_az_buf = autozero_data_40ch;
	config_parameters.s_cal_buf = calibrated_data_40ch;
	config_parameters.s_cor_buf = cor_data_40ch;
	config_parameters.sa_start  = s_array_40ch;
	config_parameters.sa_end    = &(s_array_40ch[2*SA_SIZE-1]);
	for( int i = 0; i < 2*SA_SIZE; i++ ) {
	  s_array_40ch[i].chn = i;
	  s_array_40ch[i].gain = config_parameters.gain;
	}
      }
      else {
	fprintf(stderr, "unknown mode %d\n", config_parameters.mode);
	if( ei_x_encode_atom(&result, "error") || ei_x_encode_atom(&result, "unknown_mode") ) return (-1);
	legitimate_config = 0;
      }

      if( ei_x_encode_atom(&result, "ok") || ei_x_encode_atom(&result, "configured") ) return (-1);
    }

    /* Initialize the card/carrier combination */
    else if( buffer[0] == INITIALIZE ) {
      int initialize_success = 1;
      if( ei_x_new_with_version(&result) || ei_x_encode_tuple_header(&result, 2)) return (-1);

      if( CarrierOpen(0, &config_parameters.nHandle) != S_OK ) {
	if( ei_x_encode_atom(&result, "error") || ei_x_encode_atom(&result, "open_carrier_failed") ) return (-1);
	initialize_success = 0;
      }

      if( initialize_success && GetCarrierAddress(config_parameters.nHandle, &addr) != S_OK ) {
	if( ei_x_encode_atom(&result, "error") || ei_x_encode_atom(&result, "get_carrier_address_failed") ) return (-1);
	initialize_success = 0;
      }

      if( initialize_success && CarrierInitialize(config_parameters.nHandle) != S_OK ) {
	if( ei_x_encode_atom(&result, "error") || ei_x_encode_atom(&result, "carrier_init_failed") ) return (-1);
	initialize_success = 0;
      }
      else {
	config_parameters.bCarrier = TRUE;
      }

      if( initialize_success && GetIOSAddress(config_parameters.nHandle, config_parameters.slotLetter, &addr) != S_OK ) {
	if( ei_x_encode_atom(&result, "error") || ei_x_encode_atom(&result, "get_ios_addr_failed") ) return (-1);
	initialize_success = 0;
      }
      else {
	config_parameters.brd_ptr = (struct map320*) addr;
	config_parameters.bInitialized = TRUE;
      }

      if( initialize_success ) {
	if( ei_x_encode_atom(&result, "ok") || ei_x_encode_atom(&result, "initialized") ) return (-1);
      }
    }

    /* Run the autozero routine for the cblk320 */
    else if( buffer[0] == AUTOZERO ) {
      int autozero_success = 1;
      if( ei_x_new_with_version(&result) || ei_x_encode_tuple_header(&result, 2)) return (-1);
      if( config_parameters.bInitialized == TRUE ) {
	byte temp_mode = config_parameters.mode;
	config_parameters.mode = AZV;
	ainmc320(&config_parameters);
	config_parameters.mode = temp_mode;
	if( ei_x_encode_atom(&result, "ok") || ei_x_encode_atom(&result, "finished") ) return (-1);
      }
      else {
	if( ei_x_encode_atom(&result, "error") || ei_x_encode_atom(&result, "card_uninitialized") ) return (-1) ;
      }
    }

    /* Run the autocalibrate routine for the cblk320 */
    else if( buffer[0] == AUTOCAL ) {
      if( ei_x_new_with_version(&result) || ei_x_encode_tuple_header(&result, 2)) return (-1);
      if( config_parameters.bInitialized == TRUE ) {
	byte temp_mode = config_parameters.mode;
	config_parameters.mode = CAL;
	ainmc320(&config_parameters);
	config_parameters.mode = temp_mode;
	if( ei_x_encode_atom(&result, "ok") || ei_x_encode_atom(&result, "autocalibrate") ) return (-1);
      }
      else {
	if( ei_x_encode_atom(&result, "error") || ei_x_encode_atom(&result, "card_uninitialized") ) return (-1) ;
      }
    }

    /* Read the raw ADC counts from the card */
    else if( buffer[0] == READ_CHANNELS_RAW ) {
      if( ei_x_new_with_version(&result) || ei_x_encode_tuple_header(&result, 2)) return (-1);
      if( config_parameters.bInitialized == TRUE ) {
	ainmc320(&config_parameters);
	if( config_parameters.mode == DIF ) {
	  if( ei_x_encode_atom(&result, "ok") || ei_x_encode_list_header(&result, SA_SIZE) ) return (-1);
	  for(int i = 0; i < SA_SIZE; i++) ei_x_encode_long(&result, raw_data_20ch[i]);
	  ei_x_encode_empty_list(&result);
	}
	else if( config_parameters.mode == SEI ) {
	  if( ei_x_encode_atom(&result, "ok") || ei_x_encode_list_header(&result, 2*SA_SIZE) ) return (-1);
	  for(int i = 0; i < 2*SA_SIZE; i++) ei_x_encode_long(&result, raw_data_40ch[i]);
	  ei_x_encode_empty_list(&result);
	}
	else {
	  if( ei_x_encode_atom(&result, "error") || ei_x_encode_atom(&result, "unknown_mode") ) return (-1);
	}
      }
      else {
	if( ei_x_encode_atom(&result, "error") || ei_x_encode_atom(&result, "card_uninitialized") ) return (-1) ;
      }

      if( ei_x_encode_atom(&result, "ok") || ei_x_encode_atom(&result, "read_raw") ) return (-1);
    }

    /* Read calibrated ADC counts from the card */
    else if( buffer[0] == READ_CHANNELS_COR ) {
      float z,s,u;
      if( ei_x_new_with_version(&result) || ei_x_encode_tuple_header(&result, 2)) return (-1);
      if( config_parameters.bInitialized == TRUE ) {
	ainmc320(&config_parameters);
	mccd320(&config_parameters);

	switch(config_parameters.range)
	  {
	  case RANGE_5TO5:
	    z = -5.0000;
	    s = 10.0000;
	    break;
	  case RANGE_0TO10:
	    z = 0.0000;
	    s = 10.0000;
	    break;
	  default: /* 10 to 10 */
	    z = -10.0000;
	    s = 20.0000;
	    break;
	  }

	if( config_parameters.mode == DIF ) {
	  if( ei_x_encode_atom(&result, "ok") || ei_x_encode_list_header(&result, SA_SIZE) ) return (-1);
	  for(int i = 0; i < SA_SIZE; i++) {
	    if( cor_data_20ch[i] > config_parameters.bit_constant ) u = (float)cor_data_20ch[i] - (float)CON16;
	    else u = (float) cor_data_20ch[i];
	    u = (u/(float)config_parameters.bit_constant * s + z)/(float)(s_array_20ch[i].gain);
	    ei_x_encode_double(&result, u);
	  }
	  ei_x_encode_empty_list(&result);
	}
	else if( config_parameters.mode == SEI ) {
	  if( ei_x_encode_atom(&result, "ok") || ei_x_encode_list_header(&result, 2*SA_SIZE) ) return (-1);
	  for(int i = 0; i < 2*SA_SIZE; i++) {
	    if( cor_data_40ch[i] > config_parameters.bit_constant ) u = (float)cor_data_40ch[i] - (float)CON16;
	    else u = (float) cor_data_40ch[i];
	    u = (u/(float)config_parameters.bit_constant * s + z)/(float)s_array_40ch[i].gain;
	    ei_x_encode_double(&result, u);
	  }
	  ei_x_encode_empty_list(&result);
	}
	else {
	  if( ei_x_encode_atom(&result, "error") || ei_x_encode_atom(&result, "unknown_mode") ) return (-1) ;
	}
	  
      }
      else {
	if( ei_x_encode_atom(&result, "error") || ei_x_encode_atom(&result, "card_uninitialized") ) return (-1) ;
      }
    }

    write_cmd(&result);
    ei_x_free(&result);
  }

  return 0;
}
