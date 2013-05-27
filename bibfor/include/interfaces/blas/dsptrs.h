        interface
          subroutine dsptrs(uplo,n,nrhs,ap,ipiv,b,ldb,info)
            integer :: ldb
            character(len=1) :: uplo
            integer :: n
            integer :: nrhs
            real(kind=8) :: ap(*)
            integer :: ipiv(*)
            real(kind=8) :: b(ldb,*)
            integer :: info
          end subroutine dsptrs
        end interface
