        interface
          subroutine fragex(ndim,imate,instam,instap,epsm,deps,vim,&
     &option,sigp,vip,typmod,dsidep,codret)
            integer :: ndim
            integer :: imate
            real(kind=8) :: instam
            real(kind=8) :: instap
            real(kind=8) :: epsm(6)
            real(kind=8) :: deps(6)
            real(kind=8) :: vim(2)
            character(len=16) :: option
            real(kind=8) :: sigp(6)
            real(kind=8) :: vip(2)
            character(len=8) :: typmod(*)
            real(kind=8) :: dsidep(6,6)
            integer :: codret
          end subroutine fragex
        end interface
