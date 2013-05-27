        interface
          subroutine utersa(ndim,iflup,iflum,ino,mno,jno,ivois,ma,iel,&
     &nbnv,nbsv,iavalp,iavalm,nsomm,jac,ltheta,valthe,valunt,niv,ifm,&
     &ityp,xn,yn,zn,term22,aux,jad,jadv,noe)
            integer :: ndim
            integer :: iflup
            integer :: iflum
            integer :: ino
            integer :: mno
            integer :: jno
            integer :: ivois
            character(len=8) :: ma
            integer :: iel
            integer :: nbnv
            integer :: nbsv
            integer :: iavalp
            integer :: iavalm
            integer :: nsomm
            real(kind=8) :: jac(9)
            logical :: ltheta
            real(kind=8) :: valthe
            real(kind=8) :: valunt
            integer :: niv
            integer :: ifm
            integer :: ityp
            real(kind=8) :: xn(9)
            real(kind=8) :: yn(9)
            real(kind=8) :: zn(9)
            real(kind=8) :: term22
            real(kind=8) :: aux
            integer :: jad
            integer :: jadv
            integer :: noe(9,6,3)
          end subroutine utersa
        end interface
