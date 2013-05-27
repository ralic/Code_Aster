        interface
          subroutine mdallr(resu1,resu2,basemo,nbmode,nbsauv,vecpr8,&
     &vecpc8,zcmplx)
            integer :: nbmode
            character(len=8) :: resu1
            character(len=8) :: resu2
            character(len=8) :: basemo
            integer :: nbsauv
            real(kind=8) :: vecpr8(nbmode,*)
            complex(kind=8) :: vecpc8(nbmode,*)
            logical :: zcmplx
          end subroutine mdallr
        end interface
