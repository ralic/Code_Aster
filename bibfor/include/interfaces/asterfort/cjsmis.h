        interface
          subroutine cjsmis(mod,crit,mater,nvi,epsd,deps,sigd,sigf,&
     &vind,vinf,noconv,aredec,stopnc,niter,epscon)
            character(len=8) :: mod
            real(kind=8) :: crit(*)
            real(kind=8) :: mater(14,2)
            integer :: nvi
            real(kind=8) :: epsd(6)
            real(kind=8) :: deps(6)
            real(kind=8) :: sigd(6)
            real(kind=8) :: sigf(6)
            real(kind=8) :: vind(*)
            real(kind=8) :: vinf(*)
            logical :: noconv
            logical :: aredec
            logical :: stopnc
            integer :: niter
            real(kind=8) :: epscon
          end subroutine cjsmis
        end interface
