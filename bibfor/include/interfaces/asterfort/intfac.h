        interface
          subroutine intfac(noma,nmaabs,ifq,fa,nno,lst,lsn,ndim,grad,&
     &jglsn,jglst,igeom,m,indptf,gln,glt,codret)
            integer :: ndim
            integer :: nno
            character(len=8) :: noma
            integer :: nmaabs
            integer :: ifq
            integer :: fa(6,4)
            real(kind=8) :: lst(nno)
            real(kind=8) :: lsn(nno)
            character(len=3) :: grad
            integer :: jglsn
            integer :: jglst
            integer :: igeom
            real(kind=8) :: m(ndim)
            integer :: indptf(3)
            real(kind=8) :: gln(ndim)
            real(kind=8) :: glt(ndim)
            integer :: codret
          end subroutine intfac
        end interface
