        interface
          subroutine sommma(np1,n1,n2,mat1,mat2,matres)
            integer :: np1
            integer :: n1
            integer :: n2
            real(kind=8) :: mat1(np1,*)
            real(kind=8) :: mat2(np1,*)
            real(kind=8) :: matres(np1,*)
          end subroutine sommma
        end interface
