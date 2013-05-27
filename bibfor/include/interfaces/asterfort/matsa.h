        interface
          subroutine matsa(dudx,sa1,sa2)
            real(kind=8) :: dudx(9)
            real(kind=8) :: sa1(6,9)
            real(kind=8) :: sa2(6,9)
          end subroutine matsa
        end interface
