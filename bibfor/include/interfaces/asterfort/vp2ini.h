        interface
          subroutine vp2ini(ldynam,lmasse,ldynfa,neq,nbvect,nborto,&
     &prorto,ddlexc,ddllag,alpha,beta,signes,vect,prsudg,nstoc,omeshi,&
     &solveu)
            integer :: neq
            integer :: ldynam
            integer :: lmasse
            integer :: ldynfa
            integer :: nbvect
            integer :: nborto
            real(kind=8) :: prorto
            integer :: ddlexc(*)
            integer :: ddllag(*)
            real(kind=8) :: alpha(*)
            real(kind=8) :: beta(*)
            real(kind=8) :: signes(*)
            real(kind=8) :: vect(neq,*)
            real(kind=8) :: prsudg
            integer :: nstoc
            real(kind=8) :: omeshi
            character(len=19) :: solveu
          end subroutine vp2ini
        end interface
