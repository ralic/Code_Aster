        interface
          subroutine cmatve(mat,vectin,vectou,ndim)
            integer :: ndim
            complex(kind=8) :: mat(ndim,ndim)
            complex(kind=8) :: vectin(ndim)
            complex(kind=8) :: vectou(ndim)
          end subroutine cmatve
        end interface
