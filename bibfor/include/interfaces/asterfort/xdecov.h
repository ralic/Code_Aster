        interface
          subroutine xdecov(ndim,elp,nnop,nnose,it,pintt,cnset,heavt,&
     &ncomp,lsn,fisco,igeom,nfiss,ifiss,pinter,ninter,npts,ainter,nse,&
     &cnse,heav,nfisc,nsemax)
            integer :: nfisc
            integer :: nnop
            integer :: ndim
            character(len=8) :: elp
            integer :: nnose
            integer :: it
            real(kind=8) :: pintt(*)
            integer :: cnset(*)
            integer :: heavt(*)
            integer :: ncomp
            real(kind=8) :: lsn(*)
            integer :: fisco(*)
            integer :: igeom
            integer :: nfiss
            integer :: ifiss
            real(kind=8) :: pinter(*)
            integer :: ninter
            integer :: npts
            real(kind=8) :: ainter(*)
            integer :: nse
            integer :: cnse(6,6)
            real(kind=8) :: heav(*)
            integer :: nsemax
          end subroutine xdecov
        end interface
