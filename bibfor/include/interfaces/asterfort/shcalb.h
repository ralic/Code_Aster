        interface
          subroutine shcalb(bksip,xnoe,b,ajac)
            real(kind=8) :: bksip(3,8)
            real(kind=8) :: xnoe(24)
            real(kind=8) :: b(3,8)
            real(kind=8) :: ajac
          end subroutine shcalb
        end interface
