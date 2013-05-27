        interface
          subroutine vpsorc(lmasse,ldynfa,nbeq,nbvect,nfreq,tolsor,&
     &vect,resid,workd,workl,lonwl,selec,dsor,sigma,vaux,workv,rwork,&
     &ddlexc,ddllag,neqact,maxitr,ifm,niv,priram,alpha,nconv,flage,&
     &solveu)
            integer :: nbvect
            integer :: nbeq
            integer :: lmasse
            integer :: ldynfa
            integer :: nfreq
            real(kind=8) :: tolsor
            complex(kind=8) :: vect(nbeq,*)
            complex(kind=8) :: resid(*)
            complex(kind=8) :: workd(*)
            complex(kind=8) :: workl(*)
            integer :: lonwl
            logical :: selec(nbvect)
            complex(kind=8) :: dsor(*)
            complex(kind=8) :: sigma
            complex(kind=8) :: vaux(*)
            complex(kind=8) :: workv(*)
            real(kind=8) :: rwork(*)
            integer :: ddlexc(nbeq)
            integer :: ddllag(nbeq)
            integer :: neqact
            integer :: maxitr
            integer :: ifm
            integer :: niv
            integer :: priram(8)
            real(kind=8) :: alpha
            integer :: nconv
            logical :: flage
            character(len=19) :: solveu
          end subroutine vpsorc
        end interface
