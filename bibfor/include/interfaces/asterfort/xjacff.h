        interface
          subroutine xjacff(elrefp,elrefc,elc,ndim,fpg,jinter,ifa,&
     &cface,ipg,nno,igeom,jbasec,g,jac,ffp,ffpc,dfdi,nd1,tau1,tau2)
            integer :: nno
            integer :: ndim
            character(len=8) :: elrefp
            character(len=8) :: elrefc
            character(len=8) :: elc
            character(len=8) :: fpg
            integer :: jinter
            integer :: ifa
            integer :: cface(5,3)
            integer :: ipg
            integer :: igeom
            integer :: jbasec
            real(kind=8) :: g(3)
            real(kind=8) :: jac
            real(kind=8) :: ffp(27)
            real(kind=8) :: ffpc(27)
            real(kind=8) :: dfdi(nno,ndim)
            real(kind=8) :: nd1(3)
            real(kind=8) :: tau1(ndim)
            real(kind=8) :: tau2(ndim)
          end subroutine xjacff
        end interface
