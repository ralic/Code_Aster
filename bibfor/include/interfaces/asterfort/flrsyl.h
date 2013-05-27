        interface
          subroutine flrsyl(trana,tranb,isgn,m,n,a,lda,b,ldb,c,ldc,&
     &scale,info)
            integer :: ldc
            integer :: ldb
            integer :: lda
            character(len=1) :: trana
            character(len=1) :: tranb
            integer :: isgn
            integer :: m
            integer :: n
            real(kind=8) :: a(lda,*)
            real(kind=8) :: b(ldb,*)
            real(kind=8) :: c(ldc,*)
            real(kind=8) :: scale
            integer :: info
          end subroutine flrsyl
        end interface
