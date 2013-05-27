        interface
          subroutine s2calb(bksip,xnoe,b,ajac)
            real(kind=8) :: bksip(3,20)
            real(kind=8) :: xnoe(60)
            real(kind=8) :: b(3,20)
            real(kind=8) :: ajac
          end subroutine s2calb
        end interface
