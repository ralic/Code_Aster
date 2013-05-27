        interface
          subroutine i3pdm2(epsi,n,s,nbs,p,dedans)
            real(kind=8) :: epsi
            real(kind=8) :: n(*)
            real(kind=8) :: s(3,*)
            integer :: nbs
            real(kind=8) :: p(*)
            logical :: dedans
          end subroutine i3pdm2
        end interface
