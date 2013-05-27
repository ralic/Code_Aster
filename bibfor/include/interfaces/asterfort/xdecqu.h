        interface
          subroutine xdecqu(nnose,it,ndim,cnset,jlsn,jgrlsn,igeom,&
     &pinter,ninter,npts,ainter,pmilie,nmilie,mfis)
            integer :: ndim
            integer :: nnose
            integer :: it
            integer :: cnset(*)
            integer :: jlsn
            integer :: jgrlsn
            integer :: igeom
            real(kind=8) :: pinter(*)
            integer :: ninter
            integer :: npts
            real(kind=8) :: ainter(*)
            real(kind=8) :: pmilie(*)
            integer :: nmilie
            integer :: mfis
          end subroutine xdecqu
        end interface
