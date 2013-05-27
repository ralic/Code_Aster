        interface
          subroutine s6calb(bksip,xnoe,b,ajac)
            real(kind=8) :: bksip(3,6)
            real(kind=8) :: xnoe(18)
            real(kind=8) :: b(3,6)
            real(kind=8) :: ajac
          end subroutine s6calb
        end interface
