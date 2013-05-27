        interface
          subroutine lcsans(ndim,option,sigp,dsidep)
            integer :: ndim
            character(len=16) :: option
            real(kind=8) :: sigp(6)
            real(kind=8) :: dsidep(6,6)
          end subroutine lcsans
        end interface
