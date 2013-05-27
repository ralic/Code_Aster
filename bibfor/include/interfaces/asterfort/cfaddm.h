        interface
          subroutine cfaddm(resoco,lctfd,lctf3d,posnoe,iliai,ndimg,&
     &nbnom,posnsm,coefno,tau1,tau2,norm,jeu,coornp)
            character(len=24) :: resoco
            logical :: lctfd
            logical :: lctf3d
            integer :: posnoe
            integer :: iliai
            integer :: ndimg
            integer :: nbnom
            integer :: posnsm(9)
            real(kind=8) :: coefno(9)
            real(kind=8) :: tau1(3)
            real(kind=8) :: tau2(3)
            real(kind=8) :: norm(3)
            real(kind=8) :: jeu
            real(kind=8) :: coornp(3)
          end subroutine cfaddm
        end interface
