        interface
          subroutine lkcomp(mod,imate,instam,instap,tm,tp,tref,deps,&
     &sigm,vinm,option,sigp,vinp,dside,retcom,invi)
            integer :: invi
            character(len=8) :: mod(*)
            integer :: imate
            real(kind=8) :: instam
            real(kind=8) :: instap
            real(kind=8) :: tm
            real(kind=8) :: tp
            real(kind=8) :: tref
            real(kind=8) :: deps(6)
            real(kind=8) :: sigm(6)
            real(kind=8) :: vinm(invi)
            character(len=16) :: option
            real(kind=8) :: sigp(6)
            real(kind=8) :: vinp(invi)
            real(kind=8) :: dside(6,6)
            integer :: retcom
          end subroutine lkcomp
        end interface
