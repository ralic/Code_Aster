        interface
          subroutine xmilac(ndim,igeom,ptint,tabco,tabdir,jgrlsn,p,r,&
     &ptmil,milac)
            integer :: ndim
            integer :: igeom
            real(kind=8) :: ptint(*)
            real(kind=8) :: tabco(*)
            integer :: tabdir(4)
            integer :: jgrlsn
            integer :: p
            integer :: r
            real(kind=8) :: ptmil(*)
            real(kind=8) :: milac(ndim)
          end subroutine xmilac
        end interface
