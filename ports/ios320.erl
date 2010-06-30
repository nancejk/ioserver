% Data model and functions for the Acromag IOS320.
% The c struct cblock320 is represented here as a record,
% except that it has an additional field which is the 
% binary blob that will be sent over the wire to C.  This
% saves the trouble of converting every time.

% Much of the code follows the definitions in io320.h to
% preserve meaning.

% in ioscarrier.h, unsigned char is typedefed to byte, and
% unsigned short is typedefed to word.  So the overall structure
% of the cblock320 is
% <<
% ptr ( map320 ) - 4 bytes
% byte range - 1 byte
% byte trigger
% byte mode
% word average - 2 bytes
% word channel
% word gain
% word data_mask
% long bit_constant - 4 bytes
% ptr( word ) raw buffer - 4 bytes
% ptr( word ) autozero buffer
% ptr( word ) calibration buffer
% ptr( word ) corrected buffer
% ptr( scan_array ) scan array start - 4 bytes
% ptr( scan_array ) scan array end
% char slot letter - 1 byte
% int nHandle - 4 bytes
% int Carrier open flag - 4 bytes
% int board ready flag - 4 bytes
% word control register - 2 bytes
% byte[33] id_prom - 33 bytes
% >>

% Mode and gain code definitions.
-define(DIFFERENTIAL, 1).
-define(SINGLE, 2).
-define(AUTOZERO, 3).
-define(CALIBRATION, 4).

-define(GAINX1, 1).
-define(GAINX2, 2).
-define(GAINX4, 4).
-define(GAINX8, 8).

% Range definitions
-define(RANGE5TO5, 1).
-define(RANGE10TO10, 2).
-define(RANGE0TO10, 3).

% Data correction and bitmasks
-define(BIT12, 16#FFF0).
-define(CON12, 4096). % Long int
-define(CON16, 65536). % Long int

% Trigger defs
-define(SOFTTRIG, 0).
-define(HARDTRIG, 1).

% Calibration points
-define(CALPOINT0, 20).
-define(CALPOINT1, 21).
-define(CALPOINT2, 22).
-define(CALPOINT3, 23).

% Slot defines
-define(SLOTA, 16#41).
-define(SLOTB, 16#42).
-define(SLOTC, 16#43).
-define(SLOTD, 16#44).

-record(

-module(ios320).
-author(Jared Nance).

