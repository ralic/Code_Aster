        interface
          subroutine pipeef(ndim,typmod,tau,mate,vim,epsp,epsd,a0,a1,&
     &a2,a3,etas)
            integer :: ndim
            character(len=8) :: typmod(2)
            real(kind=8) :: tau
            integer :: mate
            real(kind=8) :: vim(2)
            real(kind=8) :: epsp(6)
            real(kind=8) :: epsd(6)
            real(kind=8) :: a0
            real(kind=8) :: a1
            real(kind=8) :: a2
            real(kind=8) :: a3
            real(kind=8) :: etas
          end subroutine pipeef
        end interface
