        interface
          subroutine trnflg(nbx,vectpt,vecl,vecg)
            integer :: nbx
            real(kind=8) :: vectpt(9,3,3)
            real(kind=8) :: vecl(*)
            real(kind=8) :: vecg(*)
          end subroutine trnflg
        end interface
