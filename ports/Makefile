CSRC=cport sport ios320
CSRC_TGTS=$(add-suffix .o,$(CSRC))
ACROMAG_INC=/home/slowcontroller/ios
ACROMAG_OBJ=$(addsuffix .o, ioscarrier rsts320 ainmc320 ainsc320)
ACROMAG_TGT=$(addprefix $(ACROMAG_INC)/ios320/, $(ACROMAG_OBJ))

CFLAGS=-Wall -pedantic -std=c99 -I$(ACROMAG_INC)

all: ios320.beam ios320 iosutils.beam

ios320: ios320.o erl_comm.o
	gcc $(LDFLAGS) erl_comm.o $(ACROMAG_TGT) $< -o $@

%.o: %.c
	gcc -g $(CFLAGS) -o $@ -c $<

%.beam: %.erl
	erlc $<

clean:
	rm port.beam sport sport.o erl_comm.o cport cport.o

