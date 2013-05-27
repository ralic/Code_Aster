        interface
          subroutine rslphi(fami,kpg,ksp,loi,imat,troisk,troimu,depsmo&
     &,rigdmo,rieleq,pi,d,s1,ann,theta,acc,f,df,sig0,eps0,mexpo,dt,phi,&
     &phip,rigeq,rigm,p,overfl)
            character(*) :: fami
            integer :: kpg
            integer :: ksp
            character(len=16) :: loi
            integer :: imat
            real(kind=8) :: troisk
            real(kind=8) :: troimu
            real(kind=8) :: depsmo
            real(kind=8) :: rigdmo
            real(kind=8) :: rieleq
            real(kind=8) :: pi
            real(kind=8) :: d
            real(kind=8) :: s1
            real(kind=8) :: ann
            real(kind=8) :: theta
            real(kind=8) :: acc
            real(kind=8) :: f
            real(kind=8) :: df
            real(kind=8) :: sig0
            real(kind=8) :: eps0
            real(kind=8) :: mexpo
            real(kind=8) :: dt
            real(kind=8) :: phi
            real(kind=8) :: phip
            real(kind=8) :: rigeq
            real(kind=8) :: rigm
            real(kind=8) :: p
            logical :: overfl
          end subroutine rslphi
        end interface
