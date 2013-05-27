        interface
          subroutine maglrc(zimat,matr,delas,ecr)
            integer :: zimat
            real(kind=8) :: matr(*)
            real(kind=8) :: delas(6,6)
            real(kind=8) :: ecr(*)
          end subroutine maglrc
        end interface
