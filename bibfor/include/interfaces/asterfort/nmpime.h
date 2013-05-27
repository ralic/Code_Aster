        interface
          subroutine nmpime(fami,kpg,ksp,imate,option,xlong0,a,xlongm,&
     &dlong0,ncstpm,cstpm,vim,effnom,vip,effnop,klv,fono)
            integer :: ncstpm
            character(*) :: fami
            integer :: kpg
            integer :: ksp
            integer :: imate
            character(*) :: option
            real(kind=8) :: xlong0
            real(kind=8) :: a
            real(kind=8) :: xlongm
            real(kind=8) :: dlong0
            real(kind=8) :: cstpm(ncstpm)
            real(kind=8) :: vim(8)
            real(kind=8) :: effnom
            real(kind=8) :: vip(8)
            real(kind=8) :: effnop
            real(kind=8) :: klv(21)
            real(kind=8) :: fono(6)
          end subroutine nmpime
        end interface
