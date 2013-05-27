        interface
          subroutine getvr8(motfac,motcle,iocc,iarg,mxval,r8val,nbval)
            character(*) :: motfac
            character(*) :: motcle
            integer :: iocc
            integer :: iarg
            integer :: mxval
            real(kind=8) :: r8val(*)
            integer :: nbval
          end subroutine getvr8
        end interface
