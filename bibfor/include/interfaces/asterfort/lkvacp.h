        interface
          subroutine lkvacp(nbmat,mater,paraep,varpl)
            integer :: nbmat
            real(kind=8) :: mater(nbmat,2)
            real(kind=8) :: paraep(3)
            real(kind=8) :: varpl(4)
          end subroutine lkvacp
        end interface
