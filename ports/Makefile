CFLAGS=-Wall -pedantic

all: port.beam cport

cport: cport.o
	gcc $(LDFLAGS) $< -lpthread -o $@

%.o: %.c
	gcc -g $(CFLAGS) -o $@ -c $<

%.beam: %.erl
	erlc $<

clean:
	rm port.beam cport cport.o

