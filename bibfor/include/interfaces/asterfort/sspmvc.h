        interface
          subroutine sspmvc(n,m,mat,ad,t1,y)
            integer :: n
            integer :: m
            complex(kind=8) :: mat(*)
            integer :: ad(*)
            complex(kind=8) :: t1(*)
            complex(kind=8) :: y(*)
          end subroutine sspmvc
        end interface
