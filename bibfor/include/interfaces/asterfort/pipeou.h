        interface
          subroutine pipeou(mat,sup,sud,mup,mud,vim,tau,copilo)
            integer :: mat
            real(kind=8) :: sup(3)
            real(kind=8) :: sud(3)
            real(kind=8) :: mup(3)
            real(kind=8) :: mud(3)
            real(kind=8) :: vim(*)
            real(kind=8) :: tau
            real(kind=8) :: copilo(5)
          end subroutine pipeou
        end interface
