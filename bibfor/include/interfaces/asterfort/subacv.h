        interface
          subroutine subacv(cova,metr,jac,cnva,a)
            real(kind=8) :: cova(3,3)
            real(kind=8) :: metr(2,2)
            real(kind=8) :: jac
            real(kind=8) :: cnva(3,2)
            real(kind=8) :: a(2,2)
          end subroutine subacv
        end interface
