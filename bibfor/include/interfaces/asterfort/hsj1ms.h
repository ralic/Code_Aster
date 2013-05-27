        interface
          subroutine hsj1ms(epais,vectg,vectt,hsfm,hss,hsj1m,hsj1s)
            real(kind=8) :: epais
            real(kind=8) :: vectg(2,3)
            real(kind=8) :: vectt(3,3)
            real(kind=8) :: hsfm(3,9)
            real(kind=8) :: hss(2,9)
            real(kind=8) :: hsj1m(3,9)
            real(kind=8) :: hsj1s(2,9)
          end subroutine hsj1ms
        end interface
