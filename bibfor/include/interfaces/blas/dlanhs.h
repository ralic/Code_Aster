        interface
          function dlanhs(norm,n,a,lda,work)
            integer :: lda
            character(len=1) :: norm
            integer :: n
            real(kind=8) :: a(lda,*)
            real(kind=8) :: work(*)
            real(kind=8) :: dlanhs
          end function dlanhs
        end interface
