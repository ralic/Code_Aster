        interface
          subroutine dortvp(ndim,nomrc,d,modeli)
            integer :: ndim
            character(len=16) :: nomrc
            real(kind=8) :: d(6,6)
            character(len=2) :: modeli
          end subroutine dortvp
        end interface
