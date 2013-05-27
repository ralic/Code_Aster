        interface
          subroutine vpzbal(mat,neq,mxeq,d,k,l)
            integer :: mxeq
            real(kind=8) :: mat(mxeq,1)
            integer :: neq
            real(kind=8) :: d(1)
            integer :: k
            integer :: l
          end subroutine vpzbal
        end interface
