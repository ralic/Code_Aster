        interface
          subroutine xmmab4(ndim,nno,nnos,ffp,jac,ptknp,nfh,seuil,mu,&
     &singu,rr,coefbu,ddls,ddlm,mmat)
            integer :: ndim
            integer :: nno
            integer :: nnos
            real(kind=8) :: ffp(27)
            real(kind=8) :: jac
            real(kind=8) :: ptknp(3,3)
            integer :: nfh
            real(kind=8) :: seuil
            real(kind=8) :: mu
            integer :: singu
            real(kind=8) :: rr
            real(kind=8) :: coefbu
            integer :: ddls
            integer :: ddlm
            real(kind=8) :: mmat(216,216)
          end subroutine xmmab4
        end interface
