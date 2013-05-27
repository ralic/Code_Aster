        interface
          subroutine lkvarv(vintr,nbmat,mater,paravi)
            integer :: nbmat
            real(kind=8) :: vintr
            real(kind=8) :: mater(nbmat,2)
            real(kind=8) :: paravi(3)
          end subroutine lkvarv
        end interface
