        interface
          subroutine jacbm1(epais,vectg,vectt,matj,jm1,detj)
            real(kind=8) :: epais
            real(kind=8) :: vectg(2,3)
            real(kind=8) :: vectt(3,3)
            real(kind=8) :: matj(3,3)
            real(kind=8) :: jm1(3,3)
            real(kind=8) :: detj
          end subroutine jacbm1
        end interface
