        interface
          subroutine debcal(nomop,ligrel,nin,lchin,lpain,nout,lchout)
            integer :: nout
            integer :: nin
            character(len=16) :: nomop
            character(len=19) :: ligrel
            character(len=19) :: lchin(nin)
            character(len=8) :: lpain(nin)
            character(len=19) :: lchout(nout)
          end subroutine debcal
        end interface
