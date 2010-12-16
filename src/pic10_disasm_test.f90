!------------------------------------------------------------------------
! Copyright (c) 2010 Jose Sebastian Reguera Candal.
! This file is part of PIC10, The PIC10 microcontroller emulator.
!------------------------------------------------------------------------

! Test program for the PIC10 emulator disassembler
!
program pic10_disasm_test
  use pic10_disasm
  implicit none

  !               iiiiiiiiiiii
  call test(int(b"000000000000"), "NOP")
  call test(int(b"000000000010"), "OPTION")
  call test(int(b"000000000011"), "SLEEP")
  call test(int(b"000000000100"), "CLRWDT")
  call test(int(b"000001000000"), "CLRW")

  !               iiiibbbfffff 
  call test(int(b"010010110001"), "   BCF  17, 5")
  call test(int(b"010110110001"), "   BSF  17, 5")
  call test(int(b"011010110001"), " BTFSC  17, 5")
  call test(int(b"011110110001"), " BTFSS  17, 5")

  !               iiikkkkkkkkk
  call test(int(b"101100000001"), "  GOTO 257")

  !               iiiikkkkkkkk
  call test(int(b"111010000001"), " ANDLW 129")
  call test(int(b"100110000001"), "  CALL 129")
  call test(int(b"110110000001"), " IORLW 129")
  call test(int(b"110010000001"), " MOVLW 129")
  call test(int(b"100010000001"), " RETLW 129")
  call test(int(b"111110000001"), " XORLW 129")

  !               iiiiiiiiifff
  call test(int(b"000000000111"), "  TRIS   7")

  !               iiiiiiifffff
  call test(int(b"000001110001"), "  CLRF  17")
  call test(int(b"000000110001"), " MOVWF  17")

  !               iiiiiidfffff
  call test(int(b"000111110001"), " ADDWF  17, 1")
  call test(int(b"000101110001"), " ANDWF  17, 1")
  call test(int(b"001001110001"), "  COMF  17, 1")
  call test(int(b"000011110001"), "  DECF  17, 1")
  call test(int(b"001011110001"), "DECFSZ  17, 1")
  call test(int(b"001010110001"), "  INCF  17, 1")
  call test(int(b"001111110001"), "INCFSZ  17, 1")
  call test(int(b"000100110001"), " IORWF  17, 1")
  call test(int(b"001000110001"), "  MOVF  17, 1")
  call test(int(b"001101110001"), "   RLF  17, 1")
  call test(int(b"001100110001"), "   RRF  17, 1")
  call test(int(b"000010110001"), " SUBWF  17, 1")
  call test(int(b"001110110001"), " SWAPF  17, 1")
  call test(int(b"000110110001"), " XORWF  17, 1")

contains

  ! Disasm. the inst. and write an error if the result is not expected
  !
  subroutine test(inst, expected)
    integer, intent(in) :: inst
    character(len=*), intent(in) :: expected
    character(len=40) :: res
    res = pic10_decode(inst)
    if (res /= expected) then
       write (*,*) "Error: Expected <", expected, "> but got <", trim(res), ">"
    end if
  end subroutine test

end program pic10_disasm_test
