        interface
          subroutine cjsnor(mater,sig,x,nor,devnul,trac)
            real(kind=8) :: mater(14,2)
            real(kind=8) :: sig(6)
            real(kind=8) :: x(6)
            real(kind=8) :: nor(7)
            logical :: devnul
            logical :: trac
          end subroutine cjsnor
        end interface
