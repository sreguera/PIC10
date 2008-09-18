module pic10_emulator
  implicit none
  private

  public :: pic10, pic10_init, pic10_fini, pic10_step

  type pic10
     integer :: pc
     integer :: ir
     integer :: w
     integer, dimension(0:255) :: code
     integer, dimension(0:31) :: data
  end type pic10

  integer, parameter :: C = 1
  integer, parameter :: DC = 2
  integer, parameter :: Z = 4
  integer, parameter :: PD = 8
  integer, parameter :: TO = 16

  integer, parameter :: OP_AND = 1
  integer, parameter :: OP_IOR = 2
  integer, parameter :: OP_XOR = 3
  integer, parameter :: OP_COM = 4
  integer, parameter :: OP_INC = 5
  integer, parameter :: OP_DEC = 6

contains

  subroutine pic10_init(pic)
    type(pic10), intent(out) :: pic
    integer :: i
    pic%pc = 0
    pic%ir = 0
    pic%w = 0
    pic%code = 0
    pic%data = 0
    do i = 0, 255
       pic%code(i) = i * 10
    end do
  end subroutine pic10_init

  subroutine pic10_fini(pic)
    type(pic10), intent(inout) :: pic
  end subroutine pic10_fini

  subroutine pic10_step(pic)
    type(pic10), intent(inout) :: pic
    pic%pc = pic%pc + 1
    call pic10_exec(pic)
    pic%ir = pic%code(pic%pc)
  end subroutine pic10_step

  subroutine pic10_exec(pic)
    type(pic10), intent(inout) :: pic
    select case (ibits(pic%ir, 6, 6))
       case (b"000000")
          select case (ibits(pic%ir, 0, 6))
             case (b"000000")
                call pic10_NOP(pic)
             case (b"000010")
                call pic10_OPTION(pic)
             case (b"000011")
                call pic10_SLEEP(pic)
             case (b"000100")
                call pic10_CLRWTD(pic)
             case (b"000110":b"000111")
                call pic10_TRIS(pic)
             case (b"100000":b"111111")
                call pic10_MOVWF(pic)
             case default
                call pic10_UNKNOWN(pic)
          end select
       case (b"000001")
          select case (ibits(pic%ir, 0, 6))
             case (b"000000")
                call pic10_CLRW(pic)
             case (b"100000":b"111111")
                call pic10_CLRF(pic)
             case default
                call pic10_UNKNOWN(pic)
          end select
       case (b"000010")
          call pic10_SUBWF(pic)
       case (b"000011")
          call pic10_DECF(pic)
       case (b"000100")
          call pic10_IORWF(pic)
       case (b"000101")
          call pic10_ANDWF(pic)
       case (b"000110")
          call pic10_XORWF(pic)
       case (b"000111")
          call pic10_ADDWF(pic)
       case (b"001000")
          call pic10_MOVF(pic)
       case (b"001001")
          call pic10_COMF(pic)
       case (b"001010")
          call pic10_INCF(pic)
       case (b"001011")
          call pic10_DECFSZ(pic)
       case (b"001100")
          call pic10_RRF(pic)
       case (b"001101")
          call pic10_RLF(pic)
       case (b"001110")
          call pic10_SWAPF(pic)
       case (b"001111")
          call pic10_INCFSZ(pic)
       case (b"010000":b"010011")
          call pic10_BCF(pic)
       case (b"010100":b"010111")
          call pic10_BSF(pic)
       case (b"011000":b"011011")
          call pic10_BTFSC(pic)
       case (b"011100":b"011111")
          call pic10_BTFSS(pic)
       case (b"100000":b"100011")
          call pic10_RETLW(pic)
       case (b"100100":b"100111")
          call pic10_CALL(pic)
       case (b"101000":b"101111")
          call pic10_GOTO(pic)
       case (b"110000":b"110011")
          call pic10_MOVLW(pic)
       case (b"110100":b"110111")
          call pic10_IORLW(pic)
       case (b"111000":b"111011")
          call pic10_ANDLW(pic)
       case (b"111100":b"111111")
          call pic10_XORLW(pic)
    end select
  end subroutine pic10_exec

  subroutine pic10_ADDWF(pic)
    type(pic10), intent(inout) :: pic
    integer :: addr
    integer :: arg
    integer :: res
    addr = pic10_f_field(pic%ir)
    call pic10_rget(pic, addr, arg)
    res = arg +  pic%w 
    if (pic10_d_field(pic%ir) == 0) then
       pic%w = res
    else
       call pic10_rset(pic, addr, res)
    end if
    call pic10_status(pic, C+DC+Z)
  end subroutine pic10_ADDWF

  subroutine pic10_ANDLW(pic)
    type(pic10), intent(inout) :: pic
    call pic10_OP_LW(pic, OP_AND)
  end subroutine pic10_ANDLW

  subroutine pic10_ANDWF(pic)
    type(pic10), intent(inout) :: pic
    call pic10_OP_WF(pic, OP_AND)
  end subroutine pic10_ANDWF

  subroutine pic10_BCF(pic)
    type(pic10), intent(inout) :: pic
  end subroutine pic10_BCF

  subroutine pic10_BSF(pic)
    type(pic10), intent(inout) :: pic
  end subroutine pic10_BSF

  subroutine pic10_BTFSC(pic)
    type(pic10), intent(inout) :: pic
  end subroutine pic10_BTFSC

  subroutine pic10_BTFSS(pic)
    type(pic10), intent(inout) :: pic
  end subroutine pic10_BTFSS

  subroutine pic10_CALL(pic)
    type(pic10), intent(inout) :: pic
  end subroutine pic10_CALL

  subroutine pic10_CLRF(pic)
    type(pic10), intent(inout) :: pic
    integer :: addr
    addr = pic10_f_field(pic%ir)
    call pic10_rset(pic, addr, 0)
    call pic10_status(pic, Z)
  end subroutine pic10_CLRF

  subroutine pic10_CLRW(pic)
    type(pic10), intent(inout) :: pic
    pic%w = 0
    call pic10_status(pic, Z)
  end subroutine pic10_CLRW

  subroutine pic10_CLRWTD(pic)
    type(pic10), intent(inout) :: pic
    call pic10_status(pic, TO+PD)
  end subroutine pic10_CLRWTD

  subroutine pic10_COMF(pic)
    type(pic10), intent(inout) :: pic
    call pic10_OP_WF(pic, OP_COM)
  end subroutine pic10_COMF

  subroutine pic10_DECF(pic)
    type(pic10), intent(inout) :: pic
    call pic10_OP_WF(pic, OP_DEC)
  end subroutine pic10_DECF

  subroutine pic10_DECFSZ(pic)
    type(pic10), intent(inout) :: pic
  end subroutine pic10_DECFSZ

  subroutine pic10_GOTO(pic)
    type(pic10), intent(inout) :: pic
  end subroutine pic10_GOTO

  subroutine pic10_INCF(pic)
    type(pic10), intent(inout) :: pic
    call pic10_OP_WF(pic, OP_INC)
  end subroutine pic10_INCF

  subroutine pic10_INCFSZ(pic)
    type(pic10), intent(inout) :: pic
  end subroutine pic10_INCFSZ

  subroutine pic10_IORLW(pic)
    type(pic10), intent(inout) :: pic
    call pic10_OP_LW(pic, OP_IOR)
  end subroutine pic10_IORLW

  subroutine pic10_IORWF(pic)
    type(pic10), intent(inout) :: pic
    call pic10_OP_WF(pic, OP_IOR)
  end subroutine pic10_IORWF

  subroutine pic10_MOVF(pic)
    type(pic10), intent(inout) :: pic
    call pic10_status(pic, Z)
  end subroutine pic10_MOVF

  subroutine pic10_MOVLW(pic)
    type(pic10), intent(inout) :: pic
    pic%w = pic10_k_field(pic%ir)
  end subroutine pic10_MOVLW

  subroutine pic10_MOVWF(pic)
    type(pic10), intent(inout) :: pic
    call pic10_rset(pic, pic10_f_field(pic%ir), pic%w)
  end subroutine pic10_MOVWF

  subroutine pic10_NOP(pic)
    type(pic10), intent(inout) :: pic
  end subroutine pic10_NOP

  subroutine pic10_OPTION(pic)
    type(pic10), intent(inout) :: pic
  end subroutine pic10_OPTION

  subroutine pic10_RETLW(pic)
    type(pic10), intent(inout) :: pic
  end subroutine pic10_RETLW

  subroutine pic10_RLF(pic)
    type(pic10), intent(inout) :: pic
    call pic10_status(pic, C)
  end subroutine pic10_RLF

  subroutine pic10_RRF(pic)
    type(pic10), intent(inout) :: pic
    call pic10_status(pic, C)
  end subroutine pic10_RRF

  subroutine pic10_SLEEP(pic)
    type(pic10), intent(inout) :: pic
    call pic10_status(pic, TO+PD)
  end subroutine pic10_SLEEP

  subroutine pic10_SUBWF(pic)
    type(pic10), intent(inout) :: pic
    integer :: addr
    integer :: arg
    integer :: res
    addr = pic10_f_field(pic%ir)
    call pic10_rget(pic, addr, arg)
    res = arg - pic%w 
    if (pic10_d_field(pic%ir) == 0) then
       pic%w = res
    else
       call pic10_rset(pic, addr, res)
    end if
    call pic10_status(pic, C+DC+Z)
  end subroutine pic10_SUBWF

  subroutine pic10_SWAPF(pic)
    type(pic10), intent(inout) :: pic
    integer :: addr
    integer :: arg
    integer :: res
    addr = pic10_f_field(pic%ir)
    call pic10_rget(pic, addr, arg)
    res = ibits(arg, 4, 4) + ishft(ibits(arg, 0, 4), 4)
    if (pic10_d_field(pic%ir) == 0) then
       pic%w = res
    else
       call pic10_rset(pic, addr, res)
    end if
  end subroutine pic10_SWAPF

  subroutine pic10_TRIS(pic)
    type(pic10), intent(inout) :: pic
  end subroutine pic10_TRIS

  subroutine pic10_UNKNOWN(pic)
    type(pic10), intent(inout) :: pic
  end subroutine pic10_UNKNOWN

  subroutine pic10_XORLW(pic)
    type(pic10), intent(inout) :: pic
    call pic10_OP_LW(pic, OP_XOR)
  end subroutine pic10_XORLW

  subroutine pic10_XORWF(pic)
    type(pic10), intent(inout) :: pic
    call pic10_OP_WF(pic, OP_XOR)
  end subroutine pic10_XORWF

  subroutine pic10_OP_LW(pic, op)
    type(pic10), intent(inout) :: pic
    integer, intent(in) :: op
    integer :: arg
    integer :: res
    arg = pic10_k_field(pic%ir)
    select case (op)
       case (OP_AND)
          res = iand(arg,  pic%w)
       case (OP_IOR)
          res = ior(arg,  pic%w)
       case (OP_XOR)
          res = ieor(arg,  pic%w)
    end select
    pic%w = res
    call pic10_status(pic, Z)
  end subroutine pic10_OP_LW

  subroutine pic10_OP_WF(pic, op)
    type(pic10), intent(inout) :: pic
    integer, intent(in) :: op
    integer :: addr
    integer :: arg
    integer :: res
    addr = pic10_f_field(pic%ir)
    call pic10_rget(pic, addr, arg)
    select case (op)
       case (OP_AND)
          res = iand(arg,  pic%w)
       case (OP_IOR)
          res = ior(arg,  pic%w)
       case (OP_XOR)
          res = ieor(arg,  pic%w)
       case (OP_COM)
          res = not(arg)
       case (OP_INC)
          res = arg + 1
       case (OP_DEC)
          res = arg - 1
    end select
    if (pic10_d_field(pic%ir) == 0) then
       pic%w = res
    else
       call pic10_rset(pic, addr, res)
    end if
    call pic10_status(pic, C+DC+Z)
  end subroutine pic10_OP_WF

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

  subroutine pic10_status(pic, flags)
    type(pic10), intent(inout) :: pic
    integer, intent(in) :: flags
  end subroutine pic10_status

  subroutine pic10_rget(pic, addr, val)
    type(pic10), intent(inout) :: pic
    integer, intent(in) :: addr
    integer, intent(out) :: val
    val = pic%data(addr)
  end subroutine pic10_rget

  subroutine pic10_rset(pic, addr, val)
    type(pic10), intent(inout) :: pic
    integer, intent(in) :: addr
    integer, intent(in) :: val
    pic%data(addr) = val
  end subroutine pic10_rset

end module pic10_emulator
