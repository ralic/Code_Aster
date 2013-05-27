        interface
          subroutine varecr(gamp,nbmat,mater,parame)
            integer :: nbmat
            real(kind=8) :: gamp
            real(kind=8) :: mater(nbmat,2)
            real(kind=8) :: parame(5)
          end subroutine varecr
        end interface
