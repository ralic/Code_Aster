        interface
          subroutine utnorm(igeom,nsomm,naret,ino,poinc1,poinc2,jno,&
     &mno,zrino2,zrino1,zrjno2,zrjno1,x3,y3,hf,xn,yn,jac,laxi,jacob,ifm,&
     &niv)
            integer :: igeom
            integer :: nsomm
            integer :: naret
            integer :: ino
            real(kind=8) :: poinc1
            real(kind=8) :: poinc2
            integer :: jno
            integer :: mno
            real(kind=8) :: zrino2
            real(kind=8) :: zrino1
            real(kind=8) :: zrjno2
            real(kind=8) :: zrjno1
            real(kind=8) :: x3
            real(kind=8) :: y3
            real(kind=8) :: hf
            real(kind=8) :: xn(9)
            real(kind=8) :: yn(9)
            real(kind=8) :: jac(9)
            logical :: laxi
            real(kind=8) :: jacob
            integer :: ifm
            integer :: niv
          end subroutine utnorm
        end interface
