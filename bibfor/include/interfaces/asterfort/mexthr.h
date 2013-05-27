        interface
          subroutine mexthr(n,a,lda)
            integer :: lda
            integer :: n
            complex(kind=8) :: a(lda,*)
          end subroutine mexthr
        end interface
