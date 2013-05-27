        interface
          subroutine dxiner(nnoe,xyzg1,rho,epais,mass,cdg,inerti)
            integer :: nnoe
            real(kind=8) :: xyzg1(3,*)
            real(kind=8) :: rho
            real(kind=8) :: epais
            real(kind=8) :: mass
            real(kind=8) :: cdg(*)
            real(kind=8) :: inerti(*)
          end subroutine dxiner
        end interface
