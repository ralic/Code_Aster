        interface
          subroutine mrmult(cumul,lmat,vect,xsol,nbvect,prepos)
            character(*) :: cumul
            integer :: lmat
            real(kind=8) :: vect(*)
            real(kind=8) :: xsol(*)
            integer :: nbvect
            logical :: prepos
          end subroutine mrmult
        end interface
