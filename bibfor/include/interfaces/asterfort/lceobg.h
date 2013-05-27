        interface
          subroutine lceobg(ndim,typmod,imate,crit,epstm,depst,vim,&
     &option,sigp,vip,dsidep,proj,iret)
            integer :: ndim
            character(len=8) :: typmod(*)
            integer :: imate
            real(kind=8) :: crit(*)
            real(kind=8) :: epstm(12)
            real(kind=8) :: depst(12)
            real(kind=8) :: vim(7)
            character(len=16) :: option
            real(kind=8) :: sigp(6)
            real(kind=8) :: vip(7)
            real(kind=8) :: dsidep(6,6,2)
            real(kind=8) :: proj(6,6)
            integer :: iret
          end subroutine lceobg
        end interface
