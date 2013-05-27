        interface
          subroutine modsta(motcle,matfac,matpre,solveu,lmatm,nume,&
     &iddl,coef,neq,nbmode,zrmod)
            integer :: neq
            character(*) :: motcle
            character(*) :: matfac
            character(*) :: matpre
            character(*) :: solveu
            integer :: lmatm
            character(*) :: nume
            integer :: iddl(*)
            real(kind=8) :: coef(*)
            integer :: nbmode
            real(kind=8) :: zrmod(neq,*)
          end subroutine modsta
        end interface
