        interface
          subroutine mcopco(noma,newgeo,ndim,nummai,ksi1,ksi2,geom)
            character(len=8) :: noma
            character(len=19) :: newgeo
            integer :: ndim
            integer :: nummai
            real(kind=8) :: ksi1
            real(kind=8) :: ksi2
            real(kind=8) :: geom(3)
          end subroutine mcopco
        end interface
