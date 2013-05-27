        interface
          subroutine xmmaa4(nnol,pla,ffc,jac,cstaco,mmat)
            integer :: nnol
            integer :: pla(27)
            real(kind=8) :: ffc(8)
            real(kind=8) :: jac
            real(kind=8) :: cstaco
            real(kind=8) :: mmat(216,216)
          end subroutine xmmaa4
        end interface
