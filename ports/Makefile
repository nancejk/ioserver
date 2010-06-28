CFLAGS=-Wall -pedantic

all: port.beam cport sport

cport: cport.o erl_comm.o
	gcc $(LDFLAGS) erl_comm.o $< -lpthread -o $@

sport: sport.o erl_comm.o
	gcc $(LDFLAGS) erl_comm.o $< -lpthread -o $@

%.o: %.c
	gcc -g $(CFLAGS) -o $@ -c $<

%.beam: %.erl
	erlc $<

clean:
	rm port.beam sport sport.o erl_comm.o cport cport.o

