        interface
          subroutine cjspla(mod,crit,mater,seuili,seuild,nvi,epsd,deps&
     &,sigd,vind,sigf,vinf,mecani,nivcjs,niter,ndec,epscon,iret,trac)
            character(len=8) :: mod
            real(kind=8) :: crit(*)
            real(kind=8) :: mater(14,2)
            real(kind=8) :: seuili
            real(kind=8) :: seuild
            integer :: nvi
            real(kind=8) :: epsd(6)
            real(kind=8) :: deps(6)
            real(kind=8) :: sigd(6)
            real(kind=8) :: vind(*)
            real(kind=8) :: sigf(6)
            real(kind=8) :: vinf(*)
            character(len=6) :: mecani
            character(len=4) :: nivcjs
            integer :: niter
            integer :: ndec
            real(kind=8) :: epscon
            integer :: iret
            logical :: trac
          end subroutine cjspla
        end interface
