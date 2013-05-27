        interface
          subroutine xcface(elref,lsn,lst,jgrlsn,igeom,enr,nfiss,ifiss&
     &,fisco,nfisc,noma,nmaabs,pinter,ninter,ainter,nface,nptf,cface)
            integer :: nfisc
            character(len=8) :: elref
            real(kind=8) :: lsn(*)
            real(kind=8) :: lst(*)
            integer :: jgrlsn
            integer :: igeom
            character(len=16) :: enr
            integer :: nfiss
            integer :: ifiss
            integer :: fisco(*)
            character(len=8) :: noma
            integer :: nmaabs
            real(kind=8) :: pinter(*)
            integer :: ninter
            real(kind=8) :: ainter(*)
            integer :: nface
            integer :: nptf
            integer :: cface(5,3)
          end subroutine xcface
        end interface
