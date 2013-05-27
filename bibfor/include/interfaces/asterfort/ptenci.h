        interface
          subroutine ptenci(neq,x,mat,omeg,en,itype,kanl,idis)
            integer :: neq
            real(kind=8) :: x(*)
            real(kind=8) :: mat(neq,neq)
            real(kind=8) :: omeg
            real(kind=8) :: en(*)
            integer :: itype
            integer :: kanl
            integer :: idis
          end subroutine ptenci
        end interface
