        interface
          subroutine cfflm2(resoco,resigr,ndim,neq,nesmax,nbliac,&
     &nbliai,glitol,glimin,glimax)
            character(len=24) :: resoco
            real(kind=8) :: resigr
            integer :: ndim
            integer :: neq
            integer :: nesmax
            integer :: nbliac
            integer :: nbliai
            real(kind=8) :: glitol
            real(kind=8) :: glimin
            real(kind=8) :: glimax
          end subroutine cfflm2
        end interface
