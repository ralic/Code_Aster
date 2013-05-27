        interface
          subroutine lceobl(ndim,typmod,imate,crit,epsm,deps,vim,&
     &option,sigm,vip,dsidep,iret)
            integer :: ndim
            character(len=8) :: typmod(*)
            integer :: imate
            real(kind=8) :: crit(*)
            real(kind=8) :: epsm(6)
            real(kind=8) :: deps(6)
            real(kind=8) :: vim(7)
            character(len=16) :: option
            real(kind=8) :: sigm(6)
            real(kind=8) :: vip(7)
            real(kind=8) :: dsidep(6,6)
            integer :: iret
          end subroutine lceobl
        end interface
