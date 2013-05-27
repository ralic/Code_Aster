        interface
          subroutine routhc(hr,hi,pr,a0,dr,ior)
            real(kind=8) :: hr
            real(kind=8) :: hi
            complex(kind=8) :: pr
            real(kind=8) :: a0(*)
            real(kind=8) :: dr(*)
            integer :: ior
          end subroutine routhc
        end interface
