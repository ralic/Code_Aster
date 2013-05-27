        interface
          subroutine matcox(ndim,pp,ddt1,ddt2,ddt3,ddt4,p,nno,ddlh,&
     &ddls,jac,ffp,singu,rr,mmat)
            integer :: ndim
            real(kind=8) :: pp(3,3)
            real(kind=8) :: ddt1(3,3)
            real(kind=8) :: ddt2(3,3)
            real(kind=8) :: ddt3(3,3)
            real(kind=8) :: ddt4(3,3)
            real(kind=8) :: p(3,3)
            integer :: nno
            integer :: ddlh
            integer :: ddls
            real(kind=8) :: jac
            real(kind=8) :: ffp(27)
            integer :: singu
            real(kind=8) :: rr
            real(kind=8) :: mmat(216,216)
          end subroutine matcox
        end interface
