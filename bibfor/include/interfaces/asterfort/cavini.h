        interface
          subroutine cavini(ndim,nno,geom,vim,npg,lgpg,imate)
            integer :: lgpg
            integer :: npg
            integer :: nno
            integer :: ndim
            real(kind=8) :: geom(1:ndim,1:nno)
            real(kind=8) :: vim(1:lgpg,1:npg)
            integer :: imate
          end subroutine cavini
        end interface
