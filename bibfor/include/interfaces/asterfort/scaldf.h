        interface
          subroutine scaldf(nbfonc,nbp,nbmr,disc,vale,defm,b)
            integer :: nbmr
            integer :: nbp
            integer :: nbfonc
            real(kind=8) :: disc(nbp)
            real(kind=8) :: vale(nbp,nbfonc)
            real(kind=8) :: defm(nbp,nbmr)
            real(kind=8) :: b(nbfonc,nbmr)
          end subroutine scaldf
        end interface
