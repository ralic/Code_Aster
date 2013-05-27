        interface
          subroutine mcmult(cumul,lmat,vect,xsol,nbvect,prepos)
            character(*) :: cumul
            integer :: lmat
            complex(kind=8) :: vect(*)
            complex(kind=8) :: xsol(*)
            integer :: nbvect
            logical :: prepos
          end subroutine mcmult
        end interface
