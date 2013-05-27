        interface
          subroutine xmvec3(nnol,pla,ffc,reac,jac,cstaco,vtmp)
            integer :: nnol
            integer :: pla(27)
            real(kind=8) :: ffc(8)
            real(kind=8) :: reac
            real(kind=8) :: jac
            real(kind=8) :: cstaco
            real(kind=8) :: vtmp(400)
          end subroutine xmvec3
        end interface
