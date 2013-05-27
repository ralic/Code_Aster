        interface
          subroutine lcicma(a,la,ca,lc,cc,xa,ya,b,lb,cb,xb,yb)
            integer :: cb
            integer :: lb
            integer :: ca
            integer :: la
            real(kind=8) :: a(la,ca)
            integer :: lc
            integer :: cc
            integer :: xa
            integer :: ya
            real(kind=8) :: b(lb,cb)
            integer :: xb
            integer :: yb
          end subroutine lcicma
        end interface
