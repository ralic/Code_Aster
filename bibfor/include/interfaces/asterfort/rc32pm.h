        interface
          subroutine rc32pm(lieu,seisme,pi,mi,mse,pm,pb,pmpb)
            character(len=4) :: lieu
            logical :: seisme
            real(kind=8) :: pi
            real(kind=8) :: mi(*)
            real(kind=8) :: mse(*)
            real(kind=8) :: pm
            real(kind=8) :: pb
            real(kind=8) :: pmpb
          end subroutine rc32pm
        end interface
