        interface
          subroutine togene(icho,ia,dplmod,nbchoc,nbmode,fx,fy,fz,&
     &fgene)
            integer :: nbmode
            integer :: nbchoc
            integer :: icho
            integer :: ia
            real(kind=8) :: dplmod(nbchoc,nbmode,*)
            real(kind=8) :: fx
            real(kind=8) :: fy
            real(kind=8) :: fz
            real(kind=8) :: fgene(nbmode)
          end subroutine togene
        end interface
