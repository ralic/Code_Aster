        interface
          subroutine dzonfg(nsommx,icnc,nelcom,numeli,inno,tbelzo,&
     &nbelzo,tbnozo,nbnozo)
            integer :: nelcom
            integer :: nsommx
            integer :: icnc(nsommx+2,*)
            integer :: numeli(nelcom+2,*)
            integer :: inno
            integer :: tbelzo(1000)
            integer :: nbelzo(3)
            integer :: tbnozo(1000)
            integer :: nbnozo(3)
          end subroutine dzonfg
        end interface
