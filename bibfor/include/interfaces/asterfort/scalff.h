        interface
          subroutine scalff(nbfonc,nbp,disc,vale,a)
            integer :: nbp
            integer :: nbfonc
            real(kind=8) :: disc(nbp)
            real(kind=8) :: vale(nbp,nbfonc)
            real(kind=8) :: a(nbfonc,nbfonc)
          end subroutine scalff
        end interface
