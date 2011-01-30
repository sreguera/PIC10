!------------------------------------------------------------------------
! Copyright (c) 2010 Jose Sebastian Reguera Candal.
! This file is part of PIC10, The PIC10 microcontroller emulator.
!------------------------------------------------------------------------

! Test program for the PIC10 emulator
!
program pic10_emulator_test
  use pic10_emulator
  implicit none

  call test_addwf_0
  call test_addwf_1
  call test_bcf
  call test_bsf

contains

  subroutine test_addwf_0
    type(pic10) :: pic
    integer :: expected
    integer :: res
    call pic10_init(pic)
    !              iiiiiidfffff
    pic%ir = int(b"000111010001")
    pic%w  = 11
    pic%data(17) = 20
    expected = 31
    call pic10_step(pic)
    res = pic%w
    if (res /= expected) then
       write (*,*) "Error: addwf: Expected <", expected, "> but got <", res, ">"
    end if
  end subroutine test_addwf_0

  subroutine test_addwf_1
    type(pic10) :: pic
    integer :: expected
    integer :: res
    call pic10_init(pic)
    !              iiiiiidfffff
    pic%ir = int(b"000111110001")
    pic%w  = 11
    pic%data(17) = 20
    expected = 31
    call pic10_step(pic)
    res = pic%data(17)
    if (res /= expected) then
       write (*,*) "Error: addwf: Expected <", expected, "> but got <", res, ">"
    end if
  end subroutine test_addwf_1

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
