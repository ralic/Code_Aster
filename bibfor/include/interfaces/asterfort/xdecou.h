        interface
          subroutine xdecou(ndim,elp,nnop,nnose,it,pintt,cnset,lsn,&
     &fisco,igeom,nfiss,ifiss,pinter,ninter,npts,ainter,lonref,nfisc)
            integer :: nfisc
            integer :: nnop
            integer :: ndim
            character(len=8) :: elp
            integer :: nnose
            integer :: it
            real(kind=8) :: pintt(*)
            integer :: cnset(*)
            real(kind=8) :: lsn(*)
            integer :: fisco(*)
            integer :: igeom
            integer :: nfiss
            integer :: ifiss
            real(kind=8) :: pinter(*)
            integer :: ninter
            integer :: npts
            real(kind=8) :: ainter(*)
            real(kind=8) :: lonref
          end subroutine xdecou
        end interface
