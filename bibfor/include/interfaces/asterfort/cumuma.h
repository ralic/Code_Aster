        interface
          subroutine cumuma(i,j,pmat,coef,gmat)
            integer :: i
            integer :: j
            real(kind=8) :: pmat(6,6)
            real(kind=8) :: coef
            real(kind=8) :: gmat(18,18)
          end subroutine cumuma
        end interface
