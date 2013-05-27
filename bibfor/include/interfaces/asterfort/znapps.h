        interface
          subroutine znapps(n,kev,np,shift,v,ldv,h,ldh,resid,q,ldq,&
     &workl,workd)
            integer :: ldq
            integer :: ldh
            integer :: ldv
            integer :: np
            integer :: kev
            integer :: n
            complex(kind=8) :: shift(np)
            complex(kind=8) :: v(ldv,kev+np)
            complex(kind=8) :: h(ldh,kev+np)
            complex(kind=8) :: resid(n)
            complex(kind=8) :: q(ldq,kev+np)
            complex(kind=8) :: workl(kev+np)
            complex(kind=8) :: workd(2*n)
          end subroutine znapps
        end interface
