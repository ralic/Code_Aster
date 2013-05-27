        interface
          subroutine xjacf2(elrefp,elrefc,elc,ndim,fpg,jinter,ifa,&
     &cface,nptf,ipg,nno,igeom,jbasec,g,jac,ffp,ffpc,dfdi,nd,tau1)
            character(len=8) :: elrefp
            character(len=8) :: elrefc
            character(len=8) :: elc
            integer :: ndim
            character(len=8) :: fpg
            integer :: jinter
            integer :: ifa
            integer :: cface(5,3)
            integer :: nptf
            integer :: ipg
            integer :: nno
            integer :: igeom
            integer :: jbasec
            real(kind=8) :: g(3)
            real(kind=8) :: jac
            real(kind=8) :: ffp(27)
            real(kind=8) :: ffpc(27)
            real(kind=8) :: dfdi(27,3)
            real(kind=8) :: nd(3)
            real(kind=8) :: tau1(3)
          end subroutine xjacf2
        end interface
