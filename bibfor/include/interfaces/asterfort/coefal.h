        interface
          subroutine coefal(nom1,nom2,nom3,ncdmax,ipas,ires,borncd,&
     &nborcd,coefcd,ipas1,ires1)
            character(len=24) :: nom1
            character(len=24) :: nom2
            character(len=24) :: nom3
            integer :: ncdmax
            integer :: ipas
            integer :: ires
            real(kind=8) :: borncd(20)
            integer :: nborcd
            real(kind=8) :: coefcd(20,11)
            integer :: ipas1
            integer :: ires1
          end subroutine coefal
        end interface
