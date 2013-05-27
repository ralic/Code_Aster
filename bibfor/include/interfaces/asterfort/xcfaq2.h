        interface
          subroutine xcfaq2(jlsn,jlst,jgrlsn,igeom,noma,nmaabs,ptint,&
     &ninter,ainter,nface,nptf,cface,nbtot)
            integer :: jlsn
            integer :: jlst
            integer :: jgrlsn
            integer :: igeom
            character(len=8) :: noma
            integer :: nmaabs
            real(kind=8) :: ptint(*)
            integer :: ninter
            real(kind=8) :: ainter(*)
            integer :: nface
            integer :: nptf
            integer :: cface(5,3)
            integer :: nbtot
          end subroutine xcfaq2
        end interface
