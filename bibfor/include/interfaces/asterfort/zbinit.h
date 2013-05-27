        interface
          subroutine zbinit(f0,coef,dimmem,mem)
            integer :: dimmem
            real(kind=8) :: f0
            real(kind=8) :: coef
            real(kind=8) :: mem(2,dimmem)
          end subroutine zbinit
        end interface
