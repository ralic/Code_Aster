        interface
          subroutine dfort3(nsommx,icnc,noeu1,noeu2,tbelzo,nbelt,&
     &tbnozo,nbnoe,xy,volume,energi,pe)
            integer :: nbnoe
            integer :: nbelt
            integer :: nsommx
            integer :: icnc(nsommx+2,*)
            integer :: noeu1
            integer :: noeu2
            integer :: tbelzo(nbelt)
            integer :: tbnozo(nbnoe)
            real(kind=8) :: xy(3,*)
            real(kind=8) :: volume(*)
            real(kind=8) :: energi(*)
            real(kind=8) :: pe
          end subroutine dfort3
        end interface
