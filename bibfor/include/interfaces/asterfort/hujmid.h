        interface
          subroutine hujmid(mod,crit,mater,nvi,deps,sigd,sigf,vind,&
     &vinf,noconv,aredec,stopnc,negmul,iret,subd,loop,ndec0,indi,mectra)
            character(len=8) :: mod
            real(kind=8) :: crit(*)
            real(kind=8) :: mater(22,2)
            integer :: nvi
            real(kind=8) :: deps(6)
            real(kind=8) :: sigd(6)
            real(kind=8) :: sigf(6)
            real(kind=8) :: vind(*)
            real(kind=8) :: vinf(*)
            logical :: noconv
            logical :: aredec
            logical :: stopnc
            logical :: negmul(8)
            integer :: iret
            logical :: subd
            logical :: loop
            integer :: ndec0
            integer :: indi(7)
            logical :: mectra
          end subroutine hujmid
        end interface
