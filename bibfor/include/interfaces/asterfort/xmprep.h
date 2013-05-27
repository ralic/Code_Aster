        interface
          subroutine xmprep(cface,contac,elref,elrefc,elc,ffc,ffp,fpg,&
     &iaint,ibasec,iptint,ifa,igeom,ipgf,jac,jlst,lact,nd,ndim,ninter,&
     &nlact,nno,nnos,nptf,nvit,rr,singu,tau1,tau2)
            integer :: cface(5,3)
            integer :: contac
            character(len=8) :: elref
            character(len=8) :: elrefc
            character(len=8) :: elc
            real(kind=8) :: ffc(8)
            real(kind=8) :: ffp(27)
            character(len=8) :: fpg
            integer :: iaint
            integer :: ibasec
            integer :: iptint
            integer :: ifa
            integer :: igeom
            integer :: ipgf
            real(kind=8) :: jac
            integer :: jlst
            integer :: lact(8)
            real(kind=8) :: nd(3)
            integer :: ndim
            integer :: ninter
            integer :: nlact
            integer :: nno
            integer :: nnos
            integer :: nptf
            integer :: nvit
            real(kind=8) :: rr
            integer :: singu
            real(kind=8) :: tau1(3)
            real(kind=8) :: tau2(3)
          end subroutine xmprep
        end interface
