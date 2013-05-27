        interface
          subroutine i2rgel(epsi,s,r,f,sl,r1l,r2l,f1l,f2l,adr)
            real(kind=8) :: epsi
            real(kind=8) :: s
            real(kind=8) :: r
            integer :: f
            real(kind=8) :: sl(*)
            real(kind=8) :: r1l(*)
            real(kind=8) :: r2l(*)
            integer :: f1l(*)
            integer :: f2l(*)
            integer :: adr
          end subroutine i2rgel
        end interface
