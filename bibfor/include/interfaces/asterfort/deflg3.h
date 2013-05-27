        interface
          subroutine deflg3(gn,feta,xi,me,t,tl)
            real(kind=8) :: gn(3,3)
            real(kind=8) :: feta(4)
            real(kind=8) :: xi(3,3)
            real(kind=8) :: me(3,3,3,3)
            real(kind=8) :: t(6)
            real(kind=8) :: tl(3,3,3,3)
          end subroutine deflg3
        end interface
