        interface
          subroutine canorm(coor,normal,ndim,ityp,inorm)
            real(kind=8) :: coor(*)
            real(kind=8) :: normal(3)
            integer :: ndim
            integer :: ityp
            integer :: inorm
          end subroutine canorm
        end interface
