!------------------------------------------------------------------------
! Copyright (c) 2010 Jose Sebastian Reguera Candal.
! This file is part of PIC10, The PIC10 microcontroller emulator.
!------------------------------------------------------------------------

! Test program for the PIC10 emulator
!
program pic10_emulator_test
  use pic10_emulator
  implicit none

  !                  iiiiiidfffff
  call test_wf(int(b"000111010001"), 11, 17, 20, 31, 20) ! addwf 17, 0
  call test_wf(int(b"000111110001"), 11, 17, 20, 11, 31) ! addwf 17, 1
  call test_wf(int(b"000101110001"), 11, 17, 20, 11,  0) ! andwf 17, 1
!  call test_wf(int(b"001001110001"), 11, 17, 20, 11, 31) !  COMF 17, 1
  call test_wf(int(b"000011110001"), 11, 17, 20, 11, 19) ! decf  17, 1
!  call test_wf(int(b"001011110001"), 11, 17, 20, 11, 31) !DECFSZ 17, 1
  call test_wf(int(b"001010110001"), 11, 17, 20, 11, 21) ! incf  17, 1
!  call test_wf(int(b"001111110001"), 11, 17, 20, 11, 31) !INCFSZ 17, 1
  call test_wf(int(b"000100110001"), 11, 17, 20, 11, 31) ! iorwf 17, 1
  call test_wf(int(b"001000110001"), 11, 17, 20, 11, 20) ! movf  17, 1
!  call test_wf(int(b"001101110001"), 11, 17, 20, 11, 31) !   RLF 17, 1
!  call test_wf(int(b"001100110001"), 11, 17, 20, 11, 31) !   RRF 17, 1
  call test_wf(int(b"000010110001"), 11, 17, 20, 11,  9) ! subwf 17, 1
  call test_wf(int(b"001110110001"), 11, 17, 20, 11, 65) ! swapf 17, 1
  call test_wf(int(b"000110110001"), 11, 17, 20, 11, 31) ! xorwf 17, 1

  call test_bcf
  call test_bsf

contains

  subroutine test_wf(op, w, a, f, ew, ef)
    integer, intent(in) :: op   ! Instruction
    integer, intent(in) :: w    ! Value of w register
    integer, intent(in) :: a    ! Address of register
    integer, intent(in) :: f    ! Value of register
    integer, intent(in) :: ew   ! Expected value of w register
    integer, intent(in) :: ef   ! Expected value of register
    type(pic10) :: pic
    call pic10_init(pic)
    pic%ir = op
    pic%w  = w
    pic%data(a) = f
    call pic10_step(pic)
    if (pic%w /= ew) then
       write (*,*) "Error: Expected <", ew, "> but got <", pic%w, ">"
    end if
    if (pic%data(a) /= ef) then
       write (*,*) "Error: Expected <", ef, "> but got <", pic%data(a), ">"
    end if
  end subroutine test_wf

  subroutine test_bcf
    type(pic10) :: pic
    integer :: expected
    integer :: res
    call pic10_init(pic)
    !              iiiibbbfffff
    pic%ir = int(b"010010110001")
    pic%data(17) = int(b"11111111")
    expected = int(b"11011111")
    call pic10_step(pic)
    res = pic%data(17) 
    if (res /= expected) then
       write (*,*) "Error: bcf: Expected <", expected, "> but got <", res, ">"
    end if
  end subroutine test_bcf

  subroutine test_bsf
    type(pic10) :: pic
    integer :: expected
    integer :: res
    call pic10_init(pic)
    !              iiiibbbfffff
    pic%ir = int(b"010110110001")
    pic%data(17) = int(b"00000000")
    expected = int(b"00100000")
    call pic10_step(pic)
    res = pic%data(17) 
    if (res /= expected) then
       write (*,*) "Error: bsf: Expected <", expected, "> but got <", res, ">"
    end if
  end subroutine test_bsf

end program pic10_emulator_test
