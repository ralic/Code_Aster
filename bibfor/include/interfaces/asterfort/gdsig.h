        interface
          subroutine gdsig(fami,kpg,ksp,x0pg,petik,rot0,rotk,granc,&
     &imate,gn,gm,pn,pm)
            character(*) :: fami
            integer :: kpg
            integer :: ksp
            real(kind=8) :: x0pg(3)
            real(kind=8) :: petik(3)
            real(kind=8) :: rot0(3,3)
            real(kind=8) :: rotk(3,3)
            real(kind=8) :: granc(6)
            integer :: imate
            real(kind=8) :: gn(3)
            real(kind=8) :: gm(3)
            real(kind=8) :: pn(3)
            real(kind=8) :: pm(3)
          end subroutine gdsig
        end interface
