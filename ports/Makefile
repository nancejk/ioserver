CSRC=erl_comm ios320
E=/usr/lib/erlang/lib/erl_interface-3.6.5
EINC=$(E)/include
ELIB=$(E)/lib
CSRC_TGTS=$(addsuffix .o,$(CSRC))
CFLAGS=-Wall -pedantic -std=c99
LDFLAGS=-L. -L$(ELIB) -lerl_interface -lpthread -lei

ACROMAG_BASE=/home/slowcontroller/ios/
ACROMAG_TGTS=
ACROMAG_INC=$(addprefix -I$(ACROMAG_BASE),ios320 ioscarrier)

all: ios320 erl_comm.o

ios320: $(CSRC_TGTS)
	gcc -o $@ $(CSRC_TGTS) $(LDFLAGS)

%.o: %.c
	gcc -g $(CFLAGS) -o $@ -c $< -I$(EINC) $(ACROMAG_INC)

%.beam: %.erl
	erlc $<

clean:
	rm erl_comm.o

