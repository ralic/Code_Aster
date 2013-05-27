        interface
          subroutine calet(ndim,fm,fma,fmp,edpn1,fmam,fta,etdpn1,jm,jp&
     &)
            integer :: ndim
            real(kind=8) :: fm(3,3)
            real(kind=8) :: fma(3,3)
            real(kind=8) :: fmp(3,3)
            real(kind=8) :: edpn1(3,3)
            real(kind=8) :: fmam(3,3)
            real(kind=8) :: fta(3,3)
            real(kind=8) :: etdpn1(3,3)
            real(kind=8) :: jm
            real(kind=8) :: jp
          end subroutine calet
        end interface
