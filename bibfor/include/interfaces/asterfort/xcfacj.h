        interface
          subroutine xcfacj(ptint,ptmax,ipt,ainter,lsn,igeom,nno,ndim,&
     &nfiss,ifiss,fisco,nfisc,typma)
            integer :: nno
            real(kind=8) :: ptint(*)
            integer :: ptmax
            integer :: ipt
            real(kind=8) :: ainter(*)
            real(kind=8) :: lsn(*)
            integer :: igeom
            integer :: ndim
            integer :: nfiss
            integer :: ifiss
            integer :: fisco(*)
            integer :: nfisc
            character(len=8) :: typma
          end subroutine xcfacj
        end interface
