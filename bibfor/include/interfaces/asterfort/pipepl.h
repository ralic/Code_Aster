        interface
          subroutine pipepl(ndim,compor,typmod,tau,mate,sigm,vim,epsp,&
     &epsd,a0,a1,a2,a3,etas)
            integer :: ndim
            character(len=16) :: compor
            character(len=8) :: typmod(*)
            real(kind=8) :: tau
            integer :: mate
            real(kind=8) :: sigm(6)
            real(kind=8) :: vim(2)
            real(kind=8) :: epsp(6)
            real(kind=8) :: epsd(6)
            real(kind=8) :: a0
            real(kind=8) :: a1
            real(kind=8) :: a2
            real(kind=8) :: a3
            real(kind=8) :: etas
          end subroutine pipepl
        end interface
