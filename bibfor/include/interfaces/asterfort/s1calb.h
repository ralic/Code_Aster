        interface
          subroutine s1calb(bksip,xnoe,b,ajac)
            real(kind=8) :: bksip(3,15)
            real(kind=8) :: xnoe(45)
            real(kind=8) :: b(3,15)
            real(kind=8) :: ajac
          end subroutine s1calb
        end interface
