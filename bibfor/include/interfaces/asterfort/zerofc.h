        interface
          subroutine zerofc(f,xmin,xmax,prec,niter,dp,iret,nit)
            real(kind=8) :: f
            external f
            real(kind=8) :: xmin
            real(kind=8) :: xmax
            real(kind=8) :: prec
            integer :: niter
            real(kind=8) :: dp
            integer :: iret
            integer :: nit
          end subroutine zerofc
        end interface
