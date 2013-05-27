        interface
          subroutine zlaset(uplo,m,n,alpha,beta,a,lda)
            integer :: lda
            character(len=1) :: uplo
            integer :: m
            integer :: n
            complex(kind=8) :: alpha
            complex(kind=8) :: beta
            complex(kind=8) :: a(lda,*)
          end subroutine zlaset
        end interface
