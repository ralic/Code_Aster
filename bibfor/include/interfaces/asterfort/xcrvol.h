        interface
          subroutine xcrvol(nse,ndim,jcnse,nnose,jpint,igeom,elrefp,&
     &inoloc,nbnoma,jcesd3,jcesl3,jcesv3,numa2,ifiss,vmoin,vplus,vtot)
            integer :: nbnoma
            integer :: ndim
            integer :: nse
            integer :: jcnse
            integer :: nnose
            integer :: jpint
            integer :: igeom
            character(len=8) :: elrefp
            integer :: inoloc
            integer :: jcesd3
            integer :: jcesl3
            integer :: jcesv3
            integer :: numa2
            integer :: ifiss
            real(kind=8) :: vmoin
            real(kind=8) :: vplus
            real(kind=8) :: vtot
          end subroutine xcrvol
        end interface
