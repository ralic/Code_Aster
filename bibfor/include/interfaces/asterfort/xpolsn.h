        interface
          subroutine xpolsn(elrefp,ino,n,jlsn,jlst,ima,iad,igeom,nfiss&
     &,ndime,ndim,jconx1,jconx2,co,lsn,lst)
            integer :: nfiss
            integer :: n
            character(len=8) :: elrefp
            integer :: ino
            integer :: jlsn
            integer :: jlst
            integer :: ima
            integer :: iad
            integer :: igeom
            integer :: ndime
            integer :: ndim
            integer :: jconx1
            integer :: jconx2
            real(kind=8) :: co(3)
            real(kind=8) :: lsn(nfiss)
            real(kind=8) :: lst(nfiss)
          end subroutine xpolsn
        end interface
