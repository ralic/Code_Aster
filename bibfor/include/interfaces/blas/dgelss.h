        interface
          subroutine dgelss(m,n,nrhs,a,lda,b,ldb,s,rcond,rank,work,&
     &lwork,info)
            integer :: ldb
            integer :: lda
            integer :: m
            integer :: n
            integer :: nrhs
            real(kind=8) :: a(lda,*)
            real(kind=8) :: b(ldb,*)
            real(kind=8) :: s(*)
            real(kind=8) :: rcond
            integer :: rank
            real(kind=8) :: work(*)
            integer :: lwork
            integer :: info
          end subroutine dgelss
        end interface
