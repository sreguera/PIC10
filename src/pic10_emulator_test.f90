!------------------------------------------------------------------------
! Copyright (c) 2010 Jose Sebastian Reguera Candal.
! This file is part of PIC10, The PIC10 microcontroller emulator.
!------------------------------------------------------------------------

! Test program for the PIC10 emulator
!
program pic10_emulator_test
  use pic10_emulator
  implicit none

  call test_bcf
  call test_bsf

contains

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
       write (*,*) "Error: Expected <", expected, "> but got <", res, ">"
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
       write (*,*) "Error: Expected <", expected, "> but got <", res, ">"
    end if
  end subroutine test_bsf

end program pic10_emulator_test
