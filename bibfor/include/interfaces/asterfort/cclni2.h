        interface
          subroutine cclni2(col1,col2,n,d1,d2,coef1,t1,t2,eps,ier)
            integer :: n
            complex(kind=8) :: col1(n)
            complex(kind=8) :: col2(n)
            complex(kind=8) :: d1
            complex(kind=8) :: d2
            complex(kind=8) :: coef1
            complex(kind=8) :: t1(n)
            complex(kind=8) :: t2(n)
            real(kind=8) :: eps
            integer :: ier
          end subroutine cclni2
        end interface
