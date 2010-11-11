FC = gfortran

OBJS = pic10_emulator.o pic10_disasm.o pic10_repl.o pic10_main.o

all: pic10

pic10: $(OBJS)
	$(FC) $+ -o $@

pic10_main.o: pic10_main.f90
	$(FC) -c $<

pic10_emulator.o: pic10_emulator.f90
	$(FC) -c $<

pic10_disasm.o: pic10_disasm.f90
	$(FC) -c $<

pic10_repl.o: pic10_repl.f90
	$(FC) -c $<

clean:
	rm -f *.o *.mod
