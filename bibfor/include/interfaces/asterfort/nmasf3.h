        interface
          subroutine nmasf3(nno,nbpg1,ipoids,ivf,idfde,imate,geom,&
     &deplm,sigm,vectu,compor)
            integer :: nbpg1
            integer :: nno
            integer :: ipoids
            integer :: ivf
            integer :: idfde
            integer :: imate
            real(kind=8) :: geom(3,nno)
            real(kind=8) :: deplm(3,nno)
            real(kind=8) :: sigm(78,nbpg1)
            real(kind=8) :: vectu(3,nno)
            character(len=16) :: compor(4)
          end subroutine nmasf3
        end interface
