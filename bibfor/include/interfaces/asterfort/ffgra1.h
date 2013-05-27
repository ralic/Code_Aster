        interface
          subroutine ffgra1(nbfonc,idebit,nbp1,nbp2,long,disc,vale)
            integer :: nbp2
            integer :: nbp1
            integer :: nbfonc
            integer :: idebit
            real(kind=8) :: long
            real(kind=8) :: disc(nbp1+nbp2)
            real(kind=8) :: vale(nbp1+nbp2,nbfonc)
          end subroutine ffgra1
        end interface
