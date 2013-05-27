        interface
          subroutine lcmmro(tampon,omp,nvi,vind,vinf)
            integer :: nvi
            real(kind=8) :: tampon(*)
            real(kind=8) :: omp(3)
            real(kind=8) :: vind(nvi)
            real(kind=8) :: vinf(nvi)
          end subroutine lcmmro
        end interface
