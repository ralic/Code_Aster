        interface
          subroutine vfcdet(maxdim,ndim,a,deta)
            integer :: maxdim
            integer :: ndim
            real(kind=8) :: a(1:maxdim,1:maxdim)
            real(kind=8) :: deta
          end subroutine vfcdet
        end interface
