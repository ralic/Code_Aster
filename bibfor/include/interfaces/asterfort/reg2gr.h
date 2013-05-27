        interface
          subroutine reg2gr(imate,compor,ndim,regula,dimdef,defgep,&
     &sigp,dsde2g)
            integer :: dimdef
            integer :: ndim
            integer :: imate
            character(len=16) :: compor(*)
            integer :: regula(6)
            real(kind=8) :: defgep(dimdef)
            real(kind=8) :: sigp(ndim*ndim*ndim)
            real(kind=8) :: dsde2g(ndim*ndim*ndim,ndim*ndim*ndim)
          end subroutine reg2gr
        end interface
