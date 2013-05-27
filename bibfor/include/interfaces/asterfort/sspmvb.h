        interface
          subroutine sspmvb(n,m,mat,ad,t1,y)
            integer :: n
            integer :: m
            real(kind=8) :: mat(*)
            integer :: ad(*)
            real(kind=8) :: t1(*)
            real(kind=8) :: y(*)
          end subroutine sspmvb
        end interface
