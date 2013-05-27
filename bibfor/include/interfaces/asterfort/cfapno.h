        interface
          subroutine cfapno(noma,newgeo,defico,resoco,lctfd,lctf3d,&
     &ndimg,izone,posnoe,numnoe,coorne,posnom,tau1m,tau2m,iliai)
            character(len=8) :: noma
            character(len=19) :: newgeo
            character(len=24) :: defico
            character(len=24) :: resoco
            logical :: lctfd
            logical :: lctf3d
            integer :: ndimg
            integer :: izone
            integer :: posnoe
            integer :: numnoe
            real(kind=8) :: coorne(3)
            integer :: posnom
            real(kind=8) :: tau1m(3)
            real(kind=8) :: tau2m(3)
            integer :: iliai
          end subroutine cfapno
        end interface
