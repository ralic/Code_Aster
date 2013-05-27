        interface
          subroutine uterec(ndim,iflup,iflum,ino,mno,jno,nsomm,jac,&
     &term22,aux,ltheta,valthe,valunt,niv,ifm,xn,yn,zn,valhp,valhm,valtp&
     &,valtm,ityp,itemp,itemm,noe)
            integer :: ndim
            integer :: iflup
            integer :: iflum
            integer :: ino
            integer :: mno
            integer :: jno
            integer :: nsomm
            real(kind=8) :: jac(9)
            real(kind=8) :: term22
            real(kind=8) :: aux
            logical :: ltheta
            real(kind=8) :: valthe
            real(kind=8) :: valunt
            integer :: niv
            integer :: ifm
            real(kind=8) :: xn(9)
            real(kind=8) :: yn(9)
            real(kind=8) :: zn(9)
            real(kind=8) :: valhp(9)
            real(kind=8) :: valhm(9)
            real(kind=8) :: valtp(9)
            real(kind=8) :: valtm(9)
            integer :: ityp
            integer :: itemp
            integer :: itemm
            integer :: noe(9,6,3)
          end subroutine uterec
        end interface
