        interface
          subroutine xpoffo(ndim,ndime,elrefp,nnop,igeom,co,ff)
            integer :: nnop
            integer :: ndim
            integer :: ndime
            character(len=8) :: elrefp
            integer :: igeom
            real(kind=8) :: co(ndim)
            real(kind=8) :: ff(nnop)
          end subroutine xpoffo
        end interface
