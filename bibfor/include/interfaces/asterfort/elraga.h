        interface
          subroutine elraga(elrefz,fapz,ndim,nbpg,coopg,poipg)
            character(*) :: elrefz
            character(*) :: fapz
            integer :: ndim
            integer :: nbpg
            real(kind=8) :: coopg(*)
            real(kind=8) :: poipg(*)
          end subroutine elraga
        end interface
