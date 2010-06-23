extern "C" {
#include <ioscarrier/ioscarrier.h>
#include <ios320/ios320.h>
};

#include <fstream>
#include <iostream>
#include <unistd.h>
struct scan_array s_array[SA_SIZE]; 

using namespace std;

int main(int argc, const char** argv) {
	struct cblk320 c_block320;

	long addr; // Holds board address.

	word az_data[SA_SIZE];	/* allocate data storage area */
	word cal_data[SA_SIZE];	/* allocate data storage area */
	word raw_data[SA_SIZE];	/* allocate data storage area */
	int cor_data[SA_SIZE];	/* allocate data storage area */

	c_block320.range = RANGE_5TO5;			/* default +-5 V */
	c_block320.trigger = STRIG;				/* 0 = software triggering */
	c_block320.mode = DIF;					/* mode */
	c_block320.gain = GAIN_X1;				/* gain for analog input */
	c_block320.average = 1;					/* number of samples to average */
	c_block320.channel = 0;					/* default channel */
	c_block320.data_mask = BIT12;			/* A/D converter data mask */
	c_block320.bit_constant = CON12;		/* constant for data correction */
	c_block320.s_raw_buf = &raw_data[0];	/* raw buffer start */
	c_block320.s_az_buf = &az_data[0];		/* auto zero buffer start */
	c_block320.s_cal_buf = &cal_data[0];	/* calibration buffer start */
	c_block320.s_cor_buf = &cor_data[0];	/* corrected buffer start */
	c_block320.sa_start = &s_array[0];		/* address of start of scan array */
	c_block320.sa_end = c_block320.sa_start;/* address of end of scan array */

	c_block320.bCarrier = FALSE;			/* indicate no carrier initialized and set up yet */
	c_block320.bInitialized = FALSE;		/* indicate not ready to talk */
	c_block320.slotLetter = SLOT_A;

	for( int i = 0; i < 20; i ++ ) {
		s_array[i].gain = 1;
		s_array[i].chn = i;
	}

	if(InitCarrierLib() != S_OK) {
		std::cout << "Init carrier lib failed." << std::endl;
		exit(2);
	}

	CSTATUS carrierStatus = CarrierOpen(0,&c_block320.nHandle);

	if(carrierStatus == E_OUT_OF_CARRIERS) {
		std::cout << "Out of carriers." << std::endl;
		exit(2);		
	}

	if(carrierStatus == E_OUT_OF_MEMORY) {
		std::cout << "Could not allocate memory!" << std::endl;
		exit(2);
	}

	if(carrierStatus == ERROR) {
		std::cout << "nCarrierDeviceHandle < 0" << std::endl;
		exit(2);
	}

	GetCarrierAddress(c_block320.nHandle, &addr);
	std::cout << "board address is: " << addr << "x\n" << std::endl;
	if(CarrierInitialize(c_block320.nHandle) != S_OK) {
		std::cout << "Carrier initialization failed." << std::endl;
		exit(2);
	}
	c_block320.bCarrier = TRUE;

	if(GetIOSAddress(c_block320.nHandle, c_block320.slotLetter, &addr) == S_OK) {
		std::cout << "Board initalized.\n" << std::endl;
		c_block320.brd_ptr = (struct map320 *)addr;
		c_block320.bInitialized = TRUE;
	}
	else {
		std::cout << "Couldn't initialize board! \n" << std::endl;
		exit(2);
	}

	ofstream fobj; 
	fobj.open("output.rawdat");
	double z(-5.0);
	double s(10.0);
	double u;

	// Read the auto zero values.
	byte temp_mode = c_block320.mode;
	c_block320.mode = AZV;
	ainmc320(&c_block320);
	c_block320.mode = temp_mode;

	// Auto-calibrate
	temp_mode = c_block320.mode;
	c_block320.mode = CAL;
	ainmc320(&c_block320);
	c_block320.mode = temp_mode;

        while(true) {
		ainsc320(&c_block320);	
		sccd320(&c_block320);
		if( cor_data[0] > c_block320.bit_constant) 
		  u = (float)cor_data[0] - (float)CON16;
		else
		  u = (float)cor_data[0];
		std::cout << (u / (float)c_block320.bit_constant * s + z ) / (float)c_block320.gain << std::endl; 
		//fobj << c_block320.s_raw_buf[0] << std::endl; 
		usleep(10000);
	};

	fobj.close();
	if(c_block320.bCarrier) CarrierClose(c_block320.nHandle);
	return 0;
}
