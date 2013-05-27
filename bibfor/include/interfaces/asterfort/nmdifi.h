        interface
          subroutine nmdifi(motfac,iocc,provli,tole,nbinst,numfin)
            character(len=16) :: motfac
            integer :: iocc
            character(len=19) :: provli
            real(kind=8) :: tole
            integer :: nbinst
            integer :: numfin
          end subroutine nmdifi
        end interface
