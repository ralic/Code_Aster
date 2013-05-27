        interface
          subroutine mmreac(nbdm,ndim,nne,nnm,jgeom,jdepm,geomae,&
     &geomam)
            integer :: nbdm
            integer :: ndim
            integer :: nne
            integer :: nnm
            integer :: jgeom
            integer :: jdepm
            real(kind=8) :: geomae(9,3)
            real(kind=8) :: geomam(9,3)
          end subroutine mmreac
        end interface
