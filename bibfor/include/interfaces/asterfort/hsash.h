        interface
          subroutine hsash(vectt,dudx,hss1,hss2)
            real(kind=8) :: vectt(3,3)
            real(kind=8) :: dudx(9)
            real(kind=8) :: hss1(2,9)
            real(kind=8) :: hss2(2,9)
          end subroutine hsash
        end interface
