CSRC=cport sport ios320
CSRC_TGTS=$(add-suffix .o,$(CSRC))
ACROMAG_INC=/home/slowcontroller/ios
CFLAGS=-Wall -pedantic -std=c99 -I$(ACROMAG_INC)

all: port.beam cport sport ios320

cport: cport.o erl_comm.o
	gcc $(LDFLAGS) erl_comm.o $< -lpthread -o $@

sport: sport.o erl_comm.o
	gcc $(LDFLAGS) erl_comm.o $< -lpthread -o $@

ios320: ios320.o
	gcc $(LDFLAGS) $< -o $@

%.o: %.c
	gcc -g $(CFLAGS) -o $@ -c $<

%.beam: %.erl
	erlc $<

clean:
	rm port.beam sport sport.o erl_comm.o cport cport.o

