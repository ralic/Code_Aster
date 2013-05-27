        interface
          subroutine zlascl(type,kl,ku,cfrom,cto,m,n,a,lda,info)
            integer :: lda
            character(len=1) :: type
            integer :: kl
            integer :: ku
            real(kind=8) :: cfrom
            real(kind=8) :: cto
            integer :: m
            integer :: n
            complex(kind=8) :: a(lda,*)
            integer :: info
          end subroutine zlascl
        end interface
