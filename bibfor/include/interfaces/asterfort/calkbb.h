        interface
          subroutine calkbb(nno,ndim,w,def,dsidep,kbb)
            integer :: ndim
            integer :: nno
            real(kind=8) :: w
            real(kind=8) :: def(2*ndim,nno,ndim)
            real(kind=8) :: dsidep(2*ndim,2*ndim)
            real(kind=8) :: kbb(ndim,ndim)
          end subroutine calkbb
        end interface
