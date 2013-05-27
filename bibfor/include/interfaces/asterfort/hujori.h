        interface
          subroutine hujori(sens,nmat,reorie,angl,vec,mat)
            character(len=5) :: sens
            integer :: nmat
            logical :: reorie
            real(kind=8) :: angl(3)
            real(kind=8) :: vec(6)
            real(kind=8) :: mat(6,6)
          end subroutine hujori
        end interface
