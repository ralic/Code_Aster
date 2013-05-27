        interface
          subroutine lcsolz(a,b,ndim,n,nbscmb,iret)
            integer :: nbscmb
            integer :: ndim
            complex(kind=8) :: a(ndim,ndim)
            complex(kind=8) :: b(ndim,nbscmb)
            integer :: n
            integer :: iret
          end subroutine lcsolz
        end interface
