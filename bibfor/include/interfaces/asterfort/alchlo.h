        interface
          subroutine alchlo(opt,ligrel,nin,lpain,lchin,nout,lpaout)
            integer :: nout
            integer :: nin
            integer :: opt
            character(len=19) :: ligrel
            character(len=8) :: lpain(nin)
            character(len=19) :: lchin(nin)
            character(len=8) :: lpaout(nout)
          end subroutine alchlo
        end interface
