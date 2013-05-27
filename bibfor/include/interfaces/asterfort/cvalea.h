        interface
          subroutine cvalea(ndim,cmod,ndimax,nbmod)
            integer :: nbmod
            integer :: ndimax
            integer :: ndim
            complex(kind=8) :: cmod(ndimax,nbmod)
          end subroutine cvalea
        end interface
