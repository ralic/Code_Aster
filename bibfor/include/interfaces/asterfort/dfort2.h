        interface
          subroutine dfort2(nsommx,icnc,noeu1,tbelzo,nbelt,tbnozo,&
     &nbnozo,nbnoe,xy,aire,energi,pe)
            integer :: nbnoe
            integer :: nbelt
            integer :: nsommx
            integer :: icnc(nsommx+2,*)
            integer :: noeu1
            integer :: tbelzo(nbelt)
            integer :: tbnozo(nbnoe)
            integer :: nbnozo(3)
            real(kind=8) :: xy(3,*)
            real(kind=8) :: aire(*)
            real(kind=8) :: energi(*)
            real(kind=8) :: pe
          end subroutine dfort2
        end interface
