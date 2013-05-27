        interface
          subroutine porea1(nno,nc,deplm,deplp,geom,gamma,vecteu,pgl,&
     &xl,angp)
            integer :: nc
            integer :: nno
            real(kind=8) :: deplm(nno*nc)
            real(kind=8) :: deplp(nno*nc)
            real(kind=8) :: geom(3,nno)
            real(kind=8) :: gamma
            logical :: vecteu
            real(kind=8) :: pgl(3,3)
            real(kind=8) :: xl
            real(kind=8) :: angp(3)
          end subroutine porea1
        end interface
