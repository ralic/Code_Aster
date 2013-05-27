        interface
          subroutine nmasf2(nno,npg,ipoids,ivf,idfde,geom,typmod,sigm,&
     &dfdi,vectu)
            integer :: npg
            integer :: nno
            integer :: ipoids
            integer :: ivf
            integer :: idfde
            real(kind=8) :: geom(2,nno)
            character(len=8) :: typmod(*)
            real(kind=8) :: sigm(10,npg)
            real(kind=8) :: dfdi(nno,2)
            real(kind=8) :: vectu(2,nno)
          end subroutine nmasf2
        end interface
