        interface
          subroutine mmmjac(alias,jgeom,ff,dff,laxis,nne,ndim,jacobi)
            character(len=8) :: alias
            integer :: jgeom
            real(kind=8) :: ff(9)
            real(kind=8) :: dff(2,9)
            logical :: laxis
            integer :: nne
            integer :: ndim
            real(kind=8) :: jacobi
          end subroutine mmmjac
        end interface
