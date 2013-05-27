        interface
          subroutine burafr(vin,nvi,materd,materf,nmat,timed,timef,afr&
     &,bfr,cfr)
            integer :: nmat
            real(kind=8) :: vin(*)
            integer :: nvi
            real(kind=8) :: materd(nmat,2)
            real(kind=8) :: materf(nmat,2)
            real(kind=8) :: timed
            real(kind=8) :: timef
            real(kind=8) :: afr(6)
            real(kind=8) :: bfr(6,6)
            real(kind=8) :: cfr(6,6)
          end subroutine burafr
        end interface
