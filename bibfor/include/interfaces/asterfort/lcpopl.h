        interface
          subroutine lcpopl(loi,angmas,nmat,materd,materf,mod,deps,&
     &sigd,sigf,vind,vinf)
            integer :: nmat
            character(len=16) :: loi
            real(kind=8) :: angmas(3)
            real(kind=8) :: materd(nmat,2)
            real(kind=8) :: materf(nmat,2)
            character(len=8) :: mod
            real(kind=8) :: deps(6)
            real(kind=8) :: sigd(6)
            real(kind=8) :: sigf(*)
            real(kind=8) :: vind(*)
            real(kind=8) :: vinf(*)
          end subroutine lcpopl
        end interface
