        interface
          subroutine dgetv0(ido,bmat,itry,initv,n,j,v,ldv,resid,rnorm,&
     &ipntr,workd,ierr,alpha)
            integer :: ldv
            integer :: j
            integer :: n
            integer :: ido
            character(len=1) :: bmat
            integer :: itry
            logical :: initv
            real(kind=8) :: v(ldv,j)
            real(kind=8) :: resid(n)
            real(kind=8) :: rnorm
            integer :: ipntr(3)
            real(kind=8) :: workd(2*n)
            integer :: ierr
            real(kind=8) :: alpha
          end subroutine dgetv0
        end interface
