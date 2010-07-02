% C size type defines
-define(BYTE, 8).
-define(WORD, 16).
-define(INT, 32).
-define(LONG, 32).

% C API functions that can be called
-define(CALL_BYTEPADS, <<0:?BYTE/native-integer>>).
-define(CONFIGURE, <<1:?BYTE/native-integer>>).

% IOS320 operating modes
-define(DIFFERENTIAL, <<1:?BYTE/native-integer>>).
-define(SINGLE_ENDED, <<1:?BYTE/native-integer>>).
-define(AUTOZERO_CAL, <<3:?BYTE/native-integer>>).
-define(CALIBRATION, <<4:?BYTE/native-integer>>).

% IOS320 channel gain settings
-define(GAIN_X1, <<1:?WORD/native-integer>>).
-define(GAIN_X2, <<2:?WORD/native-integer>>).
-define(GAIN_X4, <<4:?WORD/native-integer>>).
-define(GAIN_X8, <<8:?WORD/native-integer>>).

% IOS320 input range settings
-define(RANGE5TO5, <<1:?BYTE/native-integer>>).
-define(RANGE10TO10, <<2:?BYTE/native-integer>>).
-define(RANGE0TO10, <<3:?BYTE/native-integer>>).

% IOS320 data masks
-define(BIT12, <<16#FFF0:?WORD/little-integer>>).

% IOS320 data correction constants
-define(CON12, <<4096:?LONG/native-integer>>).
-define(CON16, <<65536:?LONG/native-integer>>).

% IOS320 triggering modes
-define(SOFTTRIG, <<0:?BYTE/native-integer>>).
-define(HARDTRIG, <<1:?BYTE/native-integer>>).

% Slot definitions
-define(SLOT_A, <<16#41:?BYTE/native-integer>>).
-define(SLOT_B, <<16#42:?BYTE/native-integer>>).
-define(SLOT_C, <<16#43:?BYTE/native-integer>>).
-define(SLOT_D, <<16#44:?BYTE/native-integer>>).

% IOS320 configuration record
-record(ios320config,
	{
	  brd_ptr = <<0:?INT/native-integer>>,
	  range = ?RANGE5TO5,
	  trigger = ?SOFTTRIG,
	  mode = ?DIFFERENTIAL,
	  average = <<1:?WORD/native-integer>>,
	  channel = <<0:?WORD/native-integer>>,
	  gain = ?GAIN_X1,
	  data_mask = ?BIT12,
	  bit_constant = ?CON12,
	  raw_data_buf = <<0:?INT/native-integer>>,
	  autozero_buf = <<0:?INT/native-integer>>,
	  cal_buf = <<0:?INT/native-integer>>,
	  correct_buf = <<0:?INT/native-integer>>,
	  scan_start = <<0:?INT/native-integer>>,
	  scan_end = <<0:?INT/native-integer>>,
	  slotLetter = ?SLOT_A,
	  nHandle = <<0:?INT/native-integer>>,
	  bCarrier = <<0:?INT/native-integer>>,
	  bInitialized = <<0:?INT/native-integer>>,
	  control_reg = <<0:?INT/native-integer>>,
	  board_id_prom = <<0:264/native-integer>>
	}).
