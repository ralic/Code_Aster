        interface
          subroutine prep2(ndim,npg,g,rpa,etdpn1,sigm,jm,fda,rp,rpat,&
     &etdm,etdv,sigmam,rpt,epsm,epsmm)
            integer :: npg
            integer :: ndim
            integer :: g
            real(kind=8) :: rpa(3,3)
            real(kind=8) :: etdpn1(3,3)
            real(kind=8) :: sigm(2*ndim,npg)
            real(kind=8) :: jm
            real(kind=8) :: fda(3,3)
            real(kind=8) :: rp(3,3)
            real(kind=8) :: rpat(3,3)
            real(kind=8) :: etdm(3,3)
            real(kind=8) :: etdv(6)
            real(kind=8) :: sigmam(6)
            real(kind=8) :: rpt(3,3)
            real(kind=8) :: epsm(6)
            real(kind=8) :: epsmm(6)
          end subroutine prep2
        end interface
