        interface
          subroutine tstjac(np1,n,typj,kmod,kmod0)
            integer :: np1
            integer :: n
            integer :: typj
            real(kind=8) :: kmod(np1,*)
            real(kind=8) :: kmod0(np1,*)
          end subroutine tstjac
        end interface
