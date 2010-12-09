!------------------------------------------------------------------------
! Copyright (c) 2010 Jose Sebastian Reguera Candal.
! This file is part of PIC10, The PIC10 microcontroller emulator.
!------------------------------------------------------------------------

! Test program for the PIC10 emulator disassembler
!
program pic10_disasm_test
  use pic10_disasm
  implicit none

  call test(int(b"000000000000"), "NOP")
  call test(int(b"000000000010"), "OPTION")
  call test(int(b"000000000011"), "SLEEP")
  call test(int(b"000000000100"), "CLRWTD")

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
