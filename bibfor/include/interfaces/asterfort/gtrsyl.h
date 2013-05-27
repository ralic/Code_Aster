        interface
          subroutine gtrsyl(trana,tranb,isgn,m,n,a,lda,b,ldb,c,ldc,&
     &scale,info)
            integer :: ldc
            integer :: ldb
            integer :: lda
            character(len=1) :: trana
            character(len=1) :: tranb
            integer :: isgn
            integer :: m
            integer :: n
            complex(kind=8) :: a(lda,*)
            complex(kind=8) :: b(ldb,*)
            complex(kind=8) :: c(ldc,*)
            real(kind=8) :: scale
            integer :: info
          end subroutine gtrsyl
        end interface
