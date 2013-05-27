        interface
          subroutine burail(vin,nvi,materd,materf,nmat,timed,timef,&
     &part,bn,cn)
            integer :: nmat
            integer :: nvi
            real(kind=8) :: vin(nvi)
            real(kind=8) :: materd(nmat,2)
            real(kind=8) :: materf(nmat,2)
            real(kind=8) :: timed
            real(kind=8) :: timef
            character(len=3) :: part
            real(kind=8) :: bn
            real(kind=8) :: cn
          end subroutine burail
        end interface
