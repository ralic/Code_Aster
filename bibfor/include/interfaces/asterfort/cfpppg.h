        interface
          subroutine cfpppg(resoco,ndim,neq,nesmax,nbliac,glimin,&
     &glimax)
            character(len=24) :: resoco
            integer :: ndim
            integer :: neq
            integer :: nesmax
            integer :: nbliac
            real(kind=8) :: glimin
            real(kind=8) :: glimax
          end subroutine cfpppg
        end interface
