        interface
          subroutine wpsorn(appr,lmasse,lamor,lmatra,nbeq,nbvect,nfreq&
     &,tolsor,vect,resid,workd,workl,lonwl,selec,dsor,vpr,vpi,sigma,vaux&
     &,workv,ddlexc,ddllag,neqact,maxitr,ifm,niv,priram,alpha,nconv,&
     &flage,vaur,vauc,vaul,solveu)
            integer :: nfreq
            integer :: nbeq
            character(len=1) :: appr
            integer :: lmasse
            integer :: lamor
            integer :: lmatra
            integer :: nbvect
            real(kind=8) :: tolsor
            complex(kind=8) :: vect(nbeq,*)
            real(kind=8) :: resid(*)
            real(kind=8) :: workd(*)
            real(kind=8) :: workl(*)
            integer :: lonwl
            logical :: selec(*)
            real(kind=8) :: dsor(nfreq+1,*)
            real(kind=8) :: vpr(*)
            real(kind=8) :: vpi(*)
            complex(kind=8) :: sigma
            real(kind=8) :: vaux(*)
            real(kind=8) :: workv(*)
            integer :: ddlexc(*)
            integer :: ddllag(*)
            integer :: neqact
            integer :: maxitr
            integer :: ifm
            integer :: niv
            integer :: priram(8)
            real(kind=8) :: alpha
            integer :: nconv
            logical :: flage
            real(kind=8) :: vaur(2*nbeq,*)
            complex(kind=8) :: vauc(2*nbeq,*)
            complex(kind=8) :: vaul(2*nbeq,*)
            character(len=19) :: solveu
          end subroutine wpsorn
        end interface
