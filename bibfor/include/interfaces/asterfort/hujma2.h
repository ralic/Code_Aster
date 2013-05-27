        interface
          subroutine hujma2(mod,imat,nmat,tempf,angmas,sigd,vind,&
     &materd,materf,ndt,ndi,nvi,nr,matcst)
            integer :: nmat
            character(len=8) :: mod
            integer :: imat
            real(kind=8) :: tempf
            real(kind=8) :: angmas(3)
            real(kind=8) :: sigd(6)
            real(kind=8) :: vind(50)
            real(kind=8) :: materd(nmat,2)
            real(kind=8) :: materf(nmat,2)
            integer :: ndt
            integer :: ndi
            integer :: nvi
            integer :: nr
            character(len=3) :: matcst
          end subroutine hujma2
        end interface
