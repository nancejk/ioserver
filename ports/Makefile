ERL_LIB=/usr/lib/erlang/lib/erl_interface-3.6.5
CFLAGS=-Wall -pedantic -I/usr/local/include -I$(ERL_LIB)/include
LDFLAGS=-L. -L$(ERL_LIB)/lib

all: cport tport port.beam port_payload_test.beam

cport: cport.o
	gcc $(LDFLAGS) $< -lerl_interface -lei -lpthread -o $@

tport: tport.o
	gcc $(LDFLAGS) $< -lerl_interface -lei -lpthread -o $@

%.o: %.c
	gcc -g $(CFLAGS) -o $@ -c $<

%.beam: %.erl
	erlc $<

clean:
	rm port.beam cport cport.o

