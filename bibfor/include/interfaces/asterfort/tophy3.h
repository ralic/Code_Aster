        interface
          subroutine tophy3(icho,ia,dplmod,nbchoc,nbmode,xgene,ux,uy,&
     &uz,nbexci,psidel,coef)
            integer :: nbexci
            integer :: nbmode
            integer :: nbchoc
            integer :: icho
            integer :: ia
            real(kind=8) :: dplmod(nbchoc,nbmode,*)
            real(kind=8) :: xgene(nbmode)
            real(kind=8) :: ux
            real(kind=8) :: uy
            real(kind=8) :: uz
            real(kind=8) :: psidel(nbchoc,nbexci,*)
            real(kind=8) :: coef(nbexci)
          end subroutine tophy3
        end interface
