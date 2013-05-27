        interface
          subroutine dsygv(itype,jobz,uplo,n,a,lda,b,ldb,w,work,lwork,&
     &info)
            integer :: ldb
            integer :: lda
            integer :: itype
            character(len=1) :: jobz
            character(len=1) :: uplo
            integer :: n
            real(kind=8) :: a(lda,*)
            real(kind=8) :: b(ldb,*)
            real(kind=8) :: w(*)
            real(kind=8) :: work(*)
            integer :: lwork
            integer :: info
          end subroutine dsygv
        end interface
