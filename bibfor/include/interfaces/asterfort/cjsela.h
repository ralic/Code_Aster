        interface
          subroutine cjsela(mod,crit,materf,deps,sigd,sigf,nvi,vind,&
     &vinf,iret)
            character(len=8) :: mod
            real(kind=8) :: crit(*)
            real(kind=8) :: materf(14,2)
            real(kind=8) :: deps(6)
            real(kind=8) :: sigd(6)
            real(kind=8) :: sigf(6)
            integer :: nvi
            real(kind=8) :: vind(*)
            real(kind=8) :: vinf(*)
            integer :: iret
          end subroutine cjsela
        end interface
