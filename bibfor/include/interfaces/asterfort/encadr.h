        interface
          subroutine encadr(f,x1,x2,f1,f2,niter,xmult,iret)
            real(kind=8) :: f
            external f
            real(kind=8) :: x1
            real(kind=8) :: x2
            real(kind=8) :: f1
            real(kind=8) :: f2
            integer :: niter
            real(kind=8) :: xmult
            integer :: iret
          end subroutine encadr
        end interface
