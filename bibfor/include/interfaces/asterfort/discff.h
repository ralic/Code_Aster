        interface
          subroutine discff(nbfonc,nomfon,nbp1,nbp2,disc,vale)
            integer :: nbp2
            integer :: nbp1
            integer :: nbfonc
            character(len=8) :: nomfon(nbfonc)
            real(kind=8) :: disc(nbp1+nbp2)
            real(kind=8) :: vale(nbp1+nbp2,nbfonc)
          end subroutine discff
        end interface
