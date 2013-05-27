        interface
          subroutine cffllf(resoco,ndim,neq,nesmax,nbliac,nbliai,&
     &glitol,glimin,glimax)
            character(len=24) :: resoco
            integer :: ndim
            integer :: neq
            integer :: nesmax
            integer :: nbliac
            integer :: nbliai
            real(kind=8) :: glitol
            real(kind=8) :: glimin
            real(kind=8) :: glimax
          end subroutine cffllf
        end interface
