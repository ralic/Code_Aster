        interface
          subroutine sesqui(mat,vect,ndim,normec)
            integer :: ndim
            complex(kind=8) :: mat(*)
            complex(kind=8) :: vect(ndim)
            complex(kind=8) :: normec
          end subroutine sesqui
        end interface
