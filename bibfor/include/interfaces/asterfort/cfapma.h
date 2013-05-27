        interface
          subroutine cfapma(noma,newgeo,defico,resoco,lctfd,lctf3d,&
     &ndimg,izone,posnoe,numnoe,coorne,posmam,ksipr1,ksipr2,tau1m,tau2m,&
     &iliai)
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
            integer :: posmam
            real(kind=8) :: ksipr1
            real(kind=8) :: ksipr2
            real(kind=8) :: tau1m(3)
            real(kind=8) :: tau2m(3)
            integer :: iliai
          end subroutine cfapma
        end interface
