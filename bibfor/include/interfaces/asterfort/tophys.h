        interface
          subroutine tophys(icho,ia,dplmod,nbchoc,nbmode,xgene,ux,uy,&
     &uz)
            integer :: nbmode
            integer :: nbchoc
            integer :: icho
            integer :: ia
            real(kind=8) :: dplmod(nbchoc,nbmode,*)
            real(kind=8) :: xgene(nbmode)
            real(kind=8) :: ux
            real(kind=8) :: uy
            real(kind=8) :: uz
          end subroutine tophys
        end interface
