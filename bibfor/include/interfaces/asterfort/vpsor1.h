        interface
          subroutine vpsor1(ldynfa,nbeq,nbvect,nfreq,tolsor,vect,resid&
     &,workd,workl,lonwl,selec,dsor,fshift,vaux,workv,ddlexc,ddllag,&
     &vecddl,nbddl,neqact,maxitr,ifm,niv,priram,alpha,omecor,nconv,flage&
     &,solveu,nsta,ddlsta,vstab,csta,redem)
            integer :: lonwl
            integer :: nfreq
            integer :: nbvect
            integer :: nbeq
            integer :: ldynfa
            real(kind=8) :: tolsor
            real(kind=8) :: vect(nbeq,nbvect)
            real(kind=8) :: resid(nbeq)
            real(kind=8) :: workd(3*nbeq)
            real(kind=8) :: workl(lonwl)
            logical :: selec(nbvect)
            real(kind=8) :: dsor(nfreq+1,2)
            real(kind=8) :: fshift
            real(kind=8) :: vaux(nbeq)
            real(kind=8) :: workv(3*nbvect)
            integer :: ddlexc(nbeq)
            integer :: ddllag(nbeq)
            integer :: vecddl(nbeq)
            integer :: nbddl
            integer :: neqact
            integer :: maxitr
            integer :: ifm
            integer :: niv
            integer :: priram(8)
            real(kind=8) :: alpha
            real(kind=8) :: omecor
            integer :: nconv
            logical :: flage
            character(len=19) :: solveu
            integer :: nsta
            integer :: ddlsta(nbeq)
            real(kind=8) :: vstab(nbeq)
            real(kind=8) :: csta
            integer :: redem
          end subroutine vpsor1
        end interface
