        interface
          subroutine xinvac(elp,ndim,tabar,s,ksi)
            integer :: ndim
            character(len=8) :: elp
            real(kind=8) :: tabar(*)
            real(kind=8) :: s
            real(kind=8) :: ksi(ndim)
          end subroutine xinvac
        end interface
