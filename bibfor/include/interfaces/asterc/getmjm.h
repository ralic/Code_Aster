        interface
          subroutine getmjm(motfac,iocc,nbval,motcle,type,nbarg)
            character(*) :: motfac
            integer :: iocc
            integer :: nbval
            character(*) :: motcle(*)
            character(*) :: type(*)
            integer :: nbarg
          end subroutine getmjm
        end interface
