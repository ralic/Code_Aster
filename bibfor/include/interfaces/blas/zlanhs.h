        interface
          function zlanhs(norm,n,a,lda,work)
            integer :: lda
            character(len=1) :: norm
            integer :: n
            complex(kind=8) :: a(lda,*)
            real(kind=8) :: work(*)
            real(kind=8) :: zlanhs
          end function zlanhs
        end interface
