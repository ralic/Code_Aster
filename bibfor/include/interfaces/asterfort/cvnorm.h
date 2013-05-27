        interface
          subroutine cvnorm(mat,vect,ndim,iretou)
            integer :: ndim
            complex(kind=8) :: mat(*)
            complex(kind=8) :: vect(ndim)
            integer :: iretou
          end subroutine cvnorm
        end interface
