!------------------------------------------------------------------------
! Copyright (c) 2010 Jose Sebastian Reguera Candal.
! This file is part of PIC10, The PIC10 microcontroller emulator.
!------------------------------------------------------------------------

! Read-Eval-Print-Loop for the PIC10 emulator
!
module pic10_repl
  use pic10_emulator
  use pic10_disasm
  implicit none
  private

  public pic10_run

contains

  ! Run the Read-Eval-Print-Loop
  !
  subroutine pic10_run
    type(pic10) :: pic
    character(len=10) :: command

    write (*,*) "Welcome to the PIC10 emulator."

    call pic10_init(pic)
    do
       write (*, "(Z4.3,':',Z4.3,'> ')", advance="no") pic%pc, pic%ir
       read (*,*) command
       if (command == "s") then
          call pic10_step(pic)
       elseif (command == "d") then
          write (*,*) pic10_decode(pic%ir)
       elseif (command == "q") then
          exit
       else
          write (*,*) "Invalid command."
       end if
    end do
    call pic10_fini(pic)

    write (*,*) "Goodbye."

  end subroutine pic10_run

end module pic10_repl
