        interface
          subroutine burars(vin,nvi,materd,materf,nmat,timed,timef,an,&
     &bn,cn)
            integer :: nmat
            real(kind=8) :: vin(*)
            integer :: nvi
            real(kind=8) :: materd(nmat,2)
            real(kind=8) :: materf(nmat,2)
            real(kind=8) :: timed
            real(kind=8) :: timef
            real(kind=8) :: an
            real(kind=8) :: bn
            real(kind=8) :: cn
          end subroutine burars
        end interface
