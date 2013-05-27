        interface
          subroutine dspev(jobz,uplo,n,ap,w,z,ldz,work,info)
            integer :: ldz
            character(len=1) :: jobz
            character(len=1) :: uplo
            integer :: n
            real(kind=8) :: ap(*)
            real(kind=8) :: w(*)
            real(kind=8) :: z(ldz,*)
            real(kind=8) :: work(*)
            integer :: info
          end subroutine dspev
        end interface
