        interface
          subroutine hsame(vectt,dudx,hsm1,hsm2)
            real(kind=8) :: vectt(3,3)
            real(kind=8) :: dudx(9)
            real(kind=8) :: hsm1(3,9)
            real(kind=8) :: hsm2(3,9)
          end subroutine hsame
        end interface
