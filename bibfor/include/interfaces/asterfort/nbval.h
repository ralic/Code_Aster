        interface
          subroutine nbval(ck,cm,cmat,ndim,lambda,nb)
            complex(kind=8) :: ck(*)
            complex(kind=8) :: cm(*)
            complex(kind=8) :: cmat(*)
            integer :: ndim
            real(kind=8) :: lambda
            integer :: nb
          end subroutine nbval
        end interface
