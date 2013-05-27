        interface
          subroutine vfcfks(cont,tange,maxfa,nface,uk,dukp1,dukp2,ufa,&
     &dufa1,dufa2,c,pesa,rho,drho1,drho2,xk,xfa,maxdim,ndim,fks,dfks1,&
     &dfks2)
            integer :: ndim
            integer :: maxdim
            integer :: nface
            integer :: maxfa
            logical :: cont
            logical :: tange
            real(kind=8) :: uk
            real(kind=8) :: dukp1
            real(kind=8) :: dukp2
            real(kind=8) :: ufa(1:nface)
            real(kind=8) :: dufa1(1:nface)
            real(kind=8) :: dufa2(1:nface)
            real(kind=8) :: c(1:maxfa,1:nface)
            real(kind=8) :: pesa(ndim)
            real(kind=8) :: rho
            real(kind=8) :: drho1
            real(kind=8) :: drho2
            real(kind=8) :: xk(ndim)
            real(kind=8) :: xfa(1:maxdim,1:nface)
            real(kind=8) :: fks(nface)
            real(kind=8) :: dfks1(1+maxfa,nface)
            real(kind=8) :: dfks2(1+maxfa,nface)
          end subroutine vfcfks
        end interface
