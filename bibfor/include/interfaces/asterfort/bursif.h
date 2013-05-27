        interface
          subroutine bursif(materd,materf,nmat,an,bn,cn,deps,nr,yd,&
     &dsig)
            integer :: nr
            integer :: nmat
            real(kind=8) :: materd(nmat,2)
            real(kind=8) :: materf(nmat,2)
            real(kind=8) :: an(6)
            real(kind=8) :: bn(6,6)
            real(kind=8) :: cn(6,6)
            real(kind=8) :: deps(6)
            real(kind=8) :: yd(nr)
            real(kind=8) :: dsig(6)
          end subroutine bursif
        end interface
