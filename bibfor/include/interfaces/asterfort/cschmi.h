        interface
          subroutine cschmi(ca,ndim,cvec,cbas,ndimax,nbbas)
            integer :: nbbas
            integer :: ndimax
            integer :: ndim
            complex(kind=8) :: ca(*)
            complex(kind=8) :: cvec(ndim)
            complex(kind=8) :: cbas(ndimax,nbbas)
          end subroutine cschmi
        end interface
