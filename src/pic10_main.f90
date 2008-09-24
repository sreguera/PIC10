program pic10_main
  use pic10_emulator
  use pic10_disasm
  implicit none

  type(pic10) :: pic
  character(len=10) :: command

  write (*,*) "Welcome to the PIC10 emulator."

  call pic10_init(pic)
  do
     write (*, "(Z4.3':'Z4.3' 'A10'> ')", advance="no") &
          pic%pc, pic%ir, pic10_decode(pic%ir)
     read (*,*) command
     if (command == "s") then
        call pic10_step(pic)
     elseif (command == "q") then
        exit
     else
        write (*,*) "Invalid command."
     end if
  end do
  call pic10_fini(pic)

  write (*,*) "Goodbye."

end program pic10_main
