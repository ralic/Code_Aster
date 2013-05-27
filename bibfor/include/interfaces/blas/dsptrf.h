        interface
          subroutine dsptrf(uplo,n,ap,ipiv,info)
            character(len=1) :: uplo
            integer :: n
            real(kind=8) :: ap(*)
            integer :: ipiv(*)
            integer :: info
          end subroutine dsptrf
        end interface
