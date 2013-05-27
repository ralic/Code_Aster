        interface
          subroutine coefrl(nom1,nom2,nom3,nckmax,ipas,ires,bornck,&
     &nborck,coefck,ipas1,ires1)
            character(len=24) :: nom1
            character(len=24) :: nom2
            character(len=24) :: nom3
            integer :: nckmax
            integer :: ipas
            integer :: ires
            real(kind=8) :: bornck(20)
            integer :: nborck
            real(kind=8) :: coefck(20,11)
            integer :: ipas1
            integer :: ires1
          end subroutine coefrl
        end interface
