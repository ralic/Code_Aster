        interface
          subroutine dnapps(n,kev,np,shiftr,shifti,v,ldv,h,ldh,resid,q&
     &,ldq,workl,workd)
            integer :: ldq
            integer :: ldh
            integer :: ldv
            integer :: np
            integer :: kev
            integer :: n
            real(kind=8) :: shiftr(np)
            real(kind=8) :: shifti(np)
            real(kind=8) :: v(ldv,kev+np)
            real(kind=8) :: h(ldh,kev+np)
            real(kind=8) :: resid(n)
            real(kind=8) :: q(ldq,kev+np)
            real(kind=8) :: workl(kev+np)
            real(kind=8) :: workd(2*n)
          end subroutine dnapps
        end interface
