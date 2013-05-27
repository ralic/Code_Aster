        interface
          subroutine ngfore(nddl,neps,npg,w,b,ni2ldc,sigref,fref)
            integer :: npg
            integer :: neps
            integer :: nddl
            real(kind=8) :: w(0:npg-1)
            real(kind=8) :: b(0:neps*npg-1,nddl)
            real(kind=8) :: ni2ldc(0:neps-1)
            real(kind=8) :: sigref(0:neps-1)
            real(kind=8) :: fref(nddl)
          end subroutine ngfore
        end interface
