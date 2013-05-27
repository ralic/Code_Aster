        interface
          subroutine flexib(basmod,nbmod,flex,nl,nc,numl,numc)
            integer :: nc
            integer :: nl
            character(len=8) :: basmod
            integer :: nbmod
            real(kind=8) :: flex(nl,nc)
            integer :: numl
            integer :: numc
          end subroutine flexib
        end interface
