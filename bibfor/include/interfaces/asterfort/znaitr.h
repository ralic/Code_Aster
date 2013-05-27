        interface
          subroutine znaitr(ido,bmat,n,k,np,resid,rnorm,v,ldv,h,ldh,&
     &ipntr,workd,info,alpha)
            integer :: ldh
            integer :: ldv
            integer :: np
            integer :: k
            integer :: n
            integer :: ido
            character(len=1) :: bmat
            complex(kind=8) :: resid(n)
            real(kind=8) :: rnorm
            complex(kind=8) :: v(ldv,k+np)
            complex(kind=8) :: h(ldh,k+np)
            integer :: ipntr(3)
            complex(kind=8) :: workd(3*n)
            integer :: info
            real(kind=8) :: alpha
          end subroutine znaitr
        end interface
