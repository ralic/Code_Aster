        interface
          subroutine drudrs(parame,q,h0,sigc,dudsig)
            real(kind=8) :: parame(5)
            real(kind=8) :: q(6)
            real(kind=8) :: h0
            real(kind=8) :: sigc
            real(kind=8) :: dudsig(6)
          end subroutine drudrs
        end interface
