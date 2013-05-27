        interface
          subroutine ptenpo(n,x,mat,ep,itype,iform)
            integer :: n
            real(kind=8) :: x(*)
            real(kind=8) :: mat(n,n)
            real(kind=8) :: ep(*)
            integer :: itype
            integer :: iform
          end subroutine ptenpo
        end interface
