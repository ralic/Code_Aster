        interface
          subroutine lkvacv(nbmat,mater,paravi,varvi)
            integer :: nbmat
            real(kind=8) :: mater(nbmat,2)
            real(kind=8) :: paravi(3)
            real(kind=8) :: varvi(4)
          end subroutine lkvacv
        end interface
