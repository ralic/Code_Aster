        interface
          subroutine burard(vin,nvi,materd,materf,nmat,timed,timef,an,&
     &bn,cn)
            integer :: nmat
            integer :: nvi
            real(kind=8) :: vin(nvi)
            real(kind=8) :: materd(nmat,2)
            real(kind=8) :: materf(nmat,2)
            real(kind=8) :: timed
            real(kind=8) :: timef
            real(kind=8) :: an(6)
            real(kind=8) :: bn
            real(kind=8) :: cn
          end subroutine burard
        end interface
