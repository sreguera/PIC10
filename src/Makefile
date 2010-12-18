FC = gfortran

OBJS = pic10_emulator.o pic10_disasm.o pic10_repl.o pic10_main.o

DISASM_TEST_OBJS = pic10_disasm.o pic10_disasm_test.o

EMULATOR_TEST_OBJS = pic10_emulator.o pic10_emulator_test.o

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

disasm_test: $(DISASM_TEST_OBJS)
	$(FC) $+ -o $@
	./$@

pic10_disasm_test.o: pic10_disasm_test.f90
	$(FC) -c $<

emulator_test: $(EMULATOR_TEST_OBJS)
	$(FC) $+ -o $@
	./$@

pic10_emulator_test.o: pic10_emulator_test.f90
	$(FC) -c $<

clean:
	rm -f *.o *.mod
