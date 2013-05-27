        interface
          subroutine zlatrs(uplo,trans,diag,normin,n,a,lda,x,scale,&
     &cnorm,info)
            integer :: lda
            character(len=1) :: uplo
            character(len=1) :: trans
            character(len=1) :: diag
            character(len=1) :: normin
            integer :: n
            complex(kind=8) :: a(lda,*)
            complex(kind=8) :: x(*)
            real(kind=8) :: scale
            real(kind=8) :: cnorm(*)
            integer :: info
          end subroutine zlatrs
        end interface
