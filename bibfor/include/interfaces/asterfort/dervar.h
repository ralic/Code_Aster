        interface
          subroutine dervar(gamp,nbmat,mater,parame,derpar)
            integer :: nbmat
            real(kind=8) :: gamp
            real(kind=8) :: mater(nbmat,2)
            real(kind=8) :: parame(5)
            real(kind=8) :: derpar(4)
          end subroutine dervar
        end interface
