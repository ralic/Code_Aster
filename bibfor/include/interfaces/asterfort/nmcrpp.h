        interface
          subroutine nmcrpp(motfaz,iocc,prec,criter,tole)
            character(*) :: motfaz
            integer :: iocc
            real(kind=8) :: prec
            character(len=8) :: criter
            real(kind=8) :: tole
          end subroutine nmcrpp
        end interface
