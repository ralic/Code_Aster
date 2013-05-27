        interface
          subroutine pipeba(ndim,mate,sup,sud,vim,dtau,copilo)
            integer :: ndim
            integer :: mate
            real(kind=8) :: sup(ndim)
            real(kind=8) :: sud(ndim)
            real(kind=8) :: vim
            real(kind=8) :: dtau
            real(kind=8) :: copilo(2,3)
          end subroutine pipeba
        end interface
