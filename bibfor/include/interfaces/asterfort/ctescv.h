        interface
          subroutine ctescv(cvec1,cvec0,cvec01,cvec00,ndim,xer)
            integer :: ndim
            complex(kind=8) :: cvec1(ndim)
            complex(kind=8) :: cvec0(ndim)
            complex(kind=8) :: cvec01(ndim)
            complex(kind=8) :: cvec00(ndim)
            real(kind=8) :: xer
          end subroutine ctescv
        end interface
