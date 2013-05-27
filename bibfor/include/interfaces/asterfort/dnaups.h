        interface
          subroutine dnaups(ido,bmat,n,which,nev,tol,resid,ncv,v,ldv,&
     &iparam,ipntr,workd,workl,lworkl,info,neqact,alpha,nsta,ddlsta,&
     &vstab,csta,ldynfa,ddlexc,redem)
            integer :: lworkl
            integer :: ldv
            integer :: ncv
            integer :: n
            integer :: ido
            character(len=1) :: bmat
            character(len=2) :: which
            integer :: nev
            real(kind=8) :: tol
            real(kind=8) :: resid(n)
            real(kind=8) :: v(ldv,ncv)
            integer :: iparam(11)
            integer :: ipntr(14)
            real(kind=8) :: workd(3*n)
            real(kind=8) :: workl(lworkl)
            integer :: info
            integer :: neqact
            real(kind=8) :: alpha
            integer :: nsta
            integer :: ddlsta(n)
            real(kind=8) :: vstab(n)
            real(kind=8) :: csta
            integer :: ldynfa
            integer :: ddlexc(n)
            integer :: redem
          end subroutine dnaups
        end interface
