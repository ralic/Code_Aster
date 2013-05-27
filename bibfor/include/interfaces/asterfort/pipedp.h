        interface
          subroutine pipedp(kpg,ksp,ndim,typmod,mate,epsm,sigm,vim,&
     &epsp,epsd,elgeom,a0,a1)
            integer :: kpg
            integer :: ksp
            integer :: ndim
            character(len=8) :: typmod(*)
            integer :: mate
            real(kind=8) :: epsm(6)
            real(kind=8) :: sigm(6)
            real(kind=8) :: vim(2)
            real(kind=8) :: epsp(6)
            real(kind=8) :: epsd(6)
            real(kind=8) :: elgeom(*)
            real(kind=8) :: a0
            real(kind=8) :: a1
          end subroutine pipedp
        end interface
