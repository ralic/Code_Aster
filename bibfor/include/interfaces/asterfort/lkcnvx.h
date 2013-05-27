        interface
          subroutine lkcnvx(sigd,sigf,nvi,vind,nmat,mater,seuil,vinf)
            integer :: nmat
            integer :: nvi
            real(kind=8) :: sigd(6)
            real(kind=8) :: sigf(6)
            real(kind=8) :: vind(nvi)
            real(kind=8) :: mater(nmat,2)
            real(kind=8) :: seuil
            real(kind=8) :: vinf(nvi)
          end subroutine lkcnvx
        end interface
