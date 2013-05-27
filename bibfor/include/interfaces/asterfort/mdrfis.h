        interface
          subroutine mdrfis(nbmode,depgen,fexgen,nbnli,nbrfis,dplmod,&
     &fk,dfk,parcho,angini,vrotat,foncp,temps)
            integer :: nbnli
            integer :: nbmode
            real(kind=8) :: depgen(*)
            real(kind=8) :: fexgen(*)
            integer :: nbrfis
            real(kind=8) :: dplmod(nbnli,nbmode,*)
            character(len=8) :: fk(2)
            character(len=8) :: dfk(2)
            real(kind=8) :: parcho(nbnli,*)
            real(kind=8) :: angini
            real(kind=8) :: vrotat
            character(len=8) :: foncp
            real(kind=8) :: temps
          end subroutine mdrfis
        end interface
