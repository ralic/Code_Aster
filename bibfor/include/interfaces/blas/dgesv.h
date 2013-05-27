        interface
          subroutine dgesv(n,nrhs,a,lda,ipiv,b,ldb,info)
            integer :: ldb
            integer :: lda
            integer :: n
            integer :: nrhs
            real(kind=8) :: a(lda,*)
            integer :: ipiv(*)
            real(kind=8) :: b(ldb,*)
            integer :: info
          end subroutine dgesv
        end interface
