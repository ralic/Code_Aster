        interface
          subroutine nmasym(fami,kpg,ksp,icodma,option,xlong0,a,tmoins&
     &,tplus,dlong0,effnom,vim,effnop,vip,klv,fono)
            character(*) :: fami
            integer :: kpg
            integer :: ksp
            integer :: icodma
            character(*) :: option
            real(kind=8) :: xlong0
            real(kind=8) :: a
            real(kind=8) :: tmoins
            real(kind=8) :: tplus
            real(kind=8) :: dlong0
            real(kind=8) :: effnom
            real(kind=8) :: vim(4)
            real(kind=8) :: effnop
            real(kind=8) :: vip(4)
            real(kind=8) :: klv(21)
            real(kind=8) :: fono(6)
          end subroutine nmasym
        end interface
