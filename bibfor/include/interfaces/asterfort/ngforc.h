        interface
          subroutine ngforc(nddl,neps,npg,w,b,ni2ldc,sigmam,fint)
            integer :: npg
            integer :: neps
            integer :: nddl
            real(kind=8) :: w(0:npg-1)
            real(kind=8) :: b(neps*npg*nddl)
            real(kind=8) :: ni2ldc(0:neps-1)
            real(kind=8) :: sigmam(0:neps*npg-1)
            real(kind=8) :: fint(nddl)
          end subroutine ngforc
        end interface
