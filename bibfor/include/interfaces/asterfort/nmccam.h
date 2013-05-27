        interface
          subroutine nmccam(ndim,typmod,imate,compor,crit,instam,&
     &instap,tm,tp,tref,deps,sigm,pcrm,option,sigp,pcrp,dsidep,retcom)
            integer :: ndim
            character(len=8) :: typmod(*)
            integer :: imate
            character(len=16) :: compor(*)
            real(kind=8) :: crit(3)
            real(kind=8) :: instam
            real(kind=8) :: instap
            real(kind=8) :: tm
            real(kind=8) :: tp
            real(kind=8) :: tref
            real(kind=8) :: deps(6)
            real(kind=8) :: sigm(6)
            real(kind=8) :: pcrm(7)
            character(len=16) :: option
            real(kind=8) :: sigp(6)
            real(kind=8) :: pcrp(7)
            real(kind=8) :: dsidep(6,6)
            integer :: retcom
          end subroutine nmccam
        end interface
