        interface
          subroutine vpzhes(mat,k,l,neq,mxeq,d)
            integer :: mxeq
            integer :: neq
            real(kind=8) :: mat(mxeq,neq)
            integer :: k
            integer :: l
            real(kind=8) :: d(neq)
          end subroutine vpzhes
        end interface
