        interface
          subroutine burafi(vin,nvi,materd,materf,nmat,timed,timef,afi&
     &,bfi,cfi)
            integer :: nmat
            real(kind=8) :: vin(*)
            integer :: nvi
            real(kind=8) :: materd(nmat,2)
            real(kind=8) :: materf(nmat,2)
            real(kind=8) :: timed
            real(kind=8) :: timef
            real(kind=8) :: afi(6)
            real(kind=8) :: bfi(6,6)
            real(kind=8) :: cfi(6,6)
          end subroutine burafi
        end interface
