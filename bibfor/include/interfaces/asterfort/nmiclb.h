        interface
          subroutine nmiclb(fami,kpg,ksp,option,compor,imate,xlong0,a,&
     &tmoins,tplus,dlong0,effnom,vim,effnop,vip,klv,fono,epsm,crildc,&
     &codret)
            character(*) :: fami
            integer :: kpg
            integer :: ksp
            character(len=16) :: option
            character(len=16) :: compor(*)
            integer :: imate
            real(kind=8) :: xlong0
            real(kind=8) :: a
            real(kind=8) :: tmoins
            real(kind=8) :: tplus
            real(kind=8) :: dlong0
            real(kind=8) :: effnom
            real(kind=8) :: vim(*)
            real(kind=8) :: effnop
            real(kind=8) :: vip(*)
            real(kind=8) :: klv(21)
            real(kind=8) :: fono(6)
            real(kind=8) :: epsm
            real(kind=8) :: crildc(3)
            integer :: codret
          end subroutine nmiclb
        end interface
