        interface
          subroutine mmmtcc(phasep,nnl,wpg,ffl,jacobi,coefac,matrcc)
            character(len=9) :: phasep
            integer :: nnl
            real(kind=8) :: wpg
            real(kind=8) :: ffl(9)
            real(kind=8) :: jacobi
            real(kind=8) :: coefac
            real(kind=8) :: matrcc(9,9)
          end subroutine mmmtcc
        end interface
