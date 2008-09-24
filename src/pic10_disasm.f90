module pic10_disasm
  implicit none
  private

  public :: pic10_decode

contains

  character(len=40) function pic10_decode(inst)
    integer, intent(inout) :: inst
    select case (ibits(inst, 6, 6))
       case (b"000000")
          select case (ibits(inst, 0, 6))
             case (b"000000")
                pic10_decode = pic10_unpar("NOP", inst)
             case (b"000010")
                pic10_decode = pic10_unpar("OPTION", inst)
             case (b"000011")
                pic10_decode = pic10_unpar("SLEEP", inst)
             case (b"000100")
                pic10_decode = pic10_unpar("CLRWTD", inst)
             case (b"000110":b"000111")
                pic10_decode = pic10_par_f("TRIS", inst)
             case (b"100000":b"111111")
                pic10_decode = pic10_par_f("MOVWF", inst)
             case default
                pic10_decode = pic10_unknown("UNKNOWN", inst)
          end select
       case (b"000001")
          select case (ibits(inst, 0, 6))
             case (b"000000")
                pic10_decode = pic10_unpar("CLRW", inst)
             case (b"100000":b"111111")
                pic10_decode = pic10_par_f("CLRF", inst)
             case default
                pic10_decode = pic10_unknown("UNKNOWN", inst)
          end select
       case (b"000010")
          pic10_decode = pic10_par_fd("SUBWF", inst)
       case (b"000011")
          pic10_decode = pic10_par_fd("DECF", inst)
       case (b"000100")
          pic10_decode = pic10_par_fd("IORWF", inst)
       case (b"000101")
          pic10_decode = pic10_par_fd("ANDWF", inst)
       case (b"000110")
          pic10_decode = pic10_par_fd("XORWF", inst)
       case (b"000111")
          pic10_decode = pic10_par_fd("ADDWF", inst)
       case (b"001000")
          pic10_decode = pic10_par_fd("MOVF", inst)
       case (b"001001")
          pic10_decode = pic10_par_fd("COMF", inst)
       case (b"001010")
          pic10_decode = pic10_par_fd("INCF", inst)
       case (b"001011")
          pic10_decode = pic10_par_fd("DECFSZ", inst)
       case (b"001100")
          pic10_decode = pic10_par_fd("RRF", inst)
       case (b"001101")
          pic10_decode = pic10_par_fd("RLF", inst)
       case (b"001110")
          pic10_decode = pic10_par_fd("SWAPF", inst)
       case (b"001111")
          pic10_decode = pic10_par_fd("INCFSZ", inst)
       case (b"010000":b"010011")
          pic10_decode = pic10_par_fb("BCF", inst)
       case (b"010100":b"010111")
          pic10_decode = pic10_par_fb("BSF", inst)
       case (b"011000":b"011011")
          pic10_decode = pic10_par_fb("BTFSC", inst)
       case (b"011100":b"011111")
          pic10_decode = pic10_par_fb("BTFSS", inst)
       case (b"100000":b"100011")
          pic10_decode = pic10_par_fd("RETLW", inst)
       case (b"100100":b"100111")
          pic10_decode = pic10_par_fd("CALL", inst)
       case (b"101000":b"101111")
          pic10_decode = pic10_par_fd("GOTO", inst)
       case (b"110000":b"110011")
          pic10_decode = pic10_par_fd("MOVLW", inst)
       case (b"110100":b"110111")
          pic10_decode = pic10_par_fd("IORLW", inst)
       case (b"111000":b"111011")
          pic10_decode = pic10_par_fd("ANDLW", inst)
       case (b"111100":b"111111")
          pic10_decode = pic10_par_fd("XORLW", inst)
    end select
  end function pic10_decode

  character(len=40) function pic10_unknown(name, inst)
    character(len=*), intent(in) :: name
    integer, intent(in) :: inst
    pic10_unknown = "UNKNOWN"
  end function pic10_unknown

  character(len=40) function pic10_unpar(name, inst)
    character(len=*), intent(in) :: name
    integer, intent(in) :: inst
    pic10_unpar = name
  end function pic10_unpar

  character(len=40) function pic10_par_fd(name, inst)
    character(len=*), intent(in) :: name
    integer, intent(in) :: inst
    pic10_par_fd = name
  end function pic10_par_fd

  character(len=40) function pic10_par_f(name, inst)
    character(len=*), intent(in) :: name
    integer, intent(in) :: inst
    pic10_par_f = name
  end function pic10_par_f

  character(len=40) function pic10_par_fb(name, inst)
    character(len=*), intent(in) :: name
    integer, intent(in) :: inst
    pic10_par_fb = name
  end function pic10_par_fb

  character(len=40) function pic10_par_k(name, inst)
    character(len=*), intent(in) :: name
    integer, intent(in) :: inst
    pic10_par_k = name
  end function pic10_par_k

  character(len=40) function pic10_par_lk(name, inst)
    character(len=*), intent(in) :: name
    integer, intent(in) :: inst
    pic10_par_lk = name
  end function pic10_par_lk

  integer function pic10_f_field(inst)
    integer, intent(in) :: inst
    pic10_f_field = ibits(inst, 0, 5)
  end function pic10_f_field

  integer function pic10_d_field(inst)
    integer, intent(in) :: inst
    pic10_d_field = ibits(inst, 5, 1)
  end function pic10_d_field

  integer function pic10_b_field(inst)
    integer, intent(in) :: inst
    pic10_b_field = ibits(inst, 5, 3)
  end function pic10_b_field

  integer function pic10_k_field(inst)
    integer, intent(in) :: inst
    pic10_k_field = ibits(inst, 0, 8)
  end function pic10_k_field

  integer function pic10_lk_field(inst)
    integer, intent(in) :: inst
    pic10_lk_field = ibits(inst, 0, 9)
  end function pic10_lk_field

end module pic10_disasm
