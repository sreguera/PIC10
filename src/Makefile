
OBJS = pic10_emulator.o pic10_main.o

all: pic10

pic10: $(OBJS)
	g95 $+ -o $@

pic10_main.o: pic10_main.f90
	g95 -c $<

pic10_emulator.o: pic10_emulator.f90
	g95 -c $<

clean:
	rm -f *.o *.mod