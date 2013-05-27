        interface
          subroutine caatdb(nno,a,d,b,jac,matuu)
            integer :: nno
            real(kind=8) :: a(6,3,8)
            real(kind=8) :: d(6,6)
            real(kind=8) :: b(6,3,8)
            real(kind=8) :: jac
            real(kind=8) :: matuu(1)
          end subroutine caatdb
        end interface
