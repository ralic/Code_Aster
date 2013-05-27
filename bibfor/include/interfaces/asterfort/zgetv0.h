        interface
          subroutine zgetv0(ido,bmat,initv,n,j,v,ldv,resid,rnorm,ipntr&
     &,workd,ierr,alpha)
            integer :: ldv
            integer :: j
            integer :: n
            integer :: ido
            character(len=1) :: bmat
            logical :: initv
            complex(kind=8) :: v(ldv,j)
            complex(kind=8) :: resid(n)
            real(kind=8) :: rnorm
            integer :: ipntr(3)
            complex(kind=8) :: workd(2*n)
            integer :: ierr
            real(kind=8) :: alpha
          end subroutine zgetv0
        end interface
