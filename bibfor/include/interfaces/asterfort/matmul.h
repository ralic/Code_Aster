        interface
          subroutine matmul(a,b,n1,n2,n3,ab)
            integer :: n3
            integer :: n2
            integer :: n1
            real(kind=8) :: a(n1,n2)
            real(kind=8) :: b(n2,n3)
            real(kind=8) :: ab(n1,n3)
          end subroutine matmul
        end interface
