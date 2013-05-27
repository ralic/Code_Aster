        interface
          subroutine mcmmvr(cumul,lmat,smdi,smhc,neq,vect,xsol,nbvect,&
     &vectmp,prepos)
            integer :: nbvect
            integer :: neq
            character(*) :: cumul
            integer :: lmat
            integer :: smdi(*)
            integer(kind=4) :: smhc(*)
            complex(kind=8) :: vect(neq,nbvect)
            complex(kind=8) :: xsol(neq,nbvect)
            complex(kind=8) :: vectmp(neq)
            logical :: prepos
          end subroutine mcmmvr
        end interface
