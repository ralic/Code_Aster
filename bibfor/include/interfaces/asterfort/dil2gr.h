        interface
          subroutine dil2gr(imate,compor,ndim,regula,dimdef,defgep,&
     &sigp,dsde2g)
            integer :: dimdef
            integer :: ndim
            integer :: imate
            character(len=16) :: compor(*)
            integer :: regula(6)
            real(kind=8) :: defgep(dimdef)
            real(kind=8) :: sigp(ndim)
            real(kind=8) :: dsde2g(ndim,ndim)
          end subroutine dil2gr
        end interface
