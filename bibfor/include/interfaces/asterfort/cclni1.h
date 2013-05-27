        interface
          subroutine cclni1(col,n,d1,t,eps,ier)
            integer :: n
            complex(kind=8) :: col(n)
            complex(kind=8) :: d1
            complex(kind=8) :: t(n)
            real(kind=8) :: eps
            integer :: ier
          end subroutine cclni1
        end interface
