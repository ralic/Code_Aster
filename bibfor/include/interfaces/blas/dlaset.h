        interface
          subroutine dlaset(uplo,m,n,alpha,beta,a,lda)
            integer :: lda
            character(len=1) :: uplo
            integer :: m
            integer :: n
            real(kind=8) :: alpha
            real(kind=8) :: beta
            real(kind=8) :: a(lda,*)
          end subroutine dlaset
        end interface
