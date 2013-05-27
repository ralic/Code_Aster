        interface
          subroutine mrmmvr(cumul,lmat,smdi,smhc,lmatd,neq,neql,vect,&
     &xsol,nbvect,vectmp,prepos)
            integer :: nbvect
            integer :: neq
            character(*) :: cumul
            integer :: lmat
            integer :: smdi(*)
            integer(kind=4) :: smhc(*)
            logical :: lmatd
            integer :: neql
            real(kind=8) :: vect(neq,nbvect)
            real(kind=8) :: xsol(neq,nbvect)
            real(kind=8) :: vectmp(neq)
            logical :: prepos
          end subroutine mrmmvr
        end interface
