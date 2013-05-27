        interface
          subroutine carcou(orien,l,pgl,rayon,theta,pgl1,pgl2,pgl3,&
     &pgl4,nno,omega,icoude)
            real(kind=8) :: orien(17)
            real(kind=8) :: l
            real(kind=8) :: pgl(3,3)
            real(kind=8) :: rayon
            real(kind=8) :: theta
            real(kind=8) :: pgl1(3,3)
            real(kind=8) :: pgl2(3,3)
            real(kind=8) :: pgl3(3,3)
            real(kind=8) :: pgl4(3,3)
            integer :: nno
            real(kind=8) :: omega
            integer :: icoude
          end subroutine carcou
        end interface
