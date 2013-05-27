        interface
          subroutine wp2ini(appr,lmasse,lamor,lraide,lmatra,lmtpsc,&
     &sigma,xh,xb,optiof,prorto,nborto,nbvect,neq,lbloq,lddl,alpha,beta,&
     &signe,yh,yb,solveu)
            integer :: neq
            character(len=1) :: appr
            integer :: lmasse
            integer :: lamor
            integer :: lraide
            integer :: lmatra
            integer :: lmtpsc
            complex(kind=8) :: sigma
            real(kind=8) :: xh(*)
            real(kind=8) :: xb(*)
            character(*) :: optiof
            real(kind=8) :: prorto
            integer :: nborto
            integer :: nbvect
            integer :: lbloq(*)
            integer :: lddl(*)
            real(kind=8) :: alpha(*)
            real(kind=8) :: beta(*)
            real(kind=8) :: signe(*)
            real(kind=8) :: yh(neq,*)
            real(kind=8) :: yb(neq,*)
            character(len=19) :: solveu
          end subroutine wp2ini
        end interface
