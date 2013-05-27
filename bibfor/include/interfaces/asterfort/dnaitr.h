        interface
          subroutine dnaitr(ido,bmat,n,k,np,resid,rnorm,v,ldv,h,ldh,&
     &ipntr,workd,info,alpha)
            integer :: ldh
            integer :: ldv
            integer :: np
            integer :: k
            integer :: n
            integer :: ido
            character(len=1) :: bmat
            real(kind=8) :: resid(n)
            real(kind=8) :: rnorm
            real(kind=8) :: v(ldv,k+np)
            real(kind=8) :: h(ldh,k+np)
            integer :: ipntr(3)
            real(kind=8) :: workd(3*n)
            integer :: info
            real(kind=8) :: alpha
          end subroutine dnaitr
        end interface
