        interface
          subroutine lcdpli(mod,nvi,option,materf,sigm,deps,vim,vip,&
     &sig,dsidep,iret)
            integer :: nvi
            character(len=8) :: mod
            character(len=16) :: option
            real(kind=8) :: materf(5,2)
            real(kind=8) :: sigm(6)
            real(kind=8) :: deps(6)
            real(kind=8) :: vim(nvi)
            real(kind=8) :: vip(nvi)
            real(kind=8) :: sig(6)
            real(kind=8) :: dsidep(6,6)
            integer :: iret
          end subroutine lcdpli
        end interface
