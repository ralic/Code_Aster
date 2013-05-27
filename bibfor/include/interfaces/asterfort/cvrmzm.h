        interface
          subroutine cvrmzm(n,a,lda,b,ldb)
            integer :: ldb
            integer :: lda
            integer :: n
            real(kind=8) :: a(lda,*)
            complex(kind=8) :: b(ldb,*)
          end subroutine cvrmzm
        end interface
