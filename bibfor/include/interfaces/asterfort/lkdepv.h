        interface
          subroutine lkdepv(nbmat,mater,depsv,ddepsv,dgamv,ddgamv)
            integer :: nbmat
            real(kind=8) :: mater(nbmat,2)
            real(kind=8) :: depsv(6)
            real(kind=8) :: ddepsv(6)
            real(kind=8) :: dgamv
            real(kind=8) :: ddgamv(6)
          end subroutine lkdepv
        end interface
