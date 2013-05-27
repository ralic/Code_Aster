        interface
          subroutine dneupd(rvec,howmny,select,dr,di,z,ldz,sigmar,&
     &sigmai,workev,bmat,n,which,nev,tol,resid,ncv,v,ldv,iparam,ipntr,&
     &workd,workl,lworkl,info)
            integer :: lworkl
            integer :: ldv
            integer :: ncv
            integer :: nev
            integer :: n
            integer :: ldz
            logical :: rvec
            character(len=1) :: howmny
            logical :: select(ncv)
            real(kind=8) :: dr(nev+1)
            real(kind=8) :: di(nev+1)
            real(kind=8) :: z(ldz,*)
            real(kind=8) :: sigmar
            real(kind=8) :: sigmai
            real(kind=8) :: workev(3*ncv)
            character(len=1) :: bmat
            character(len=2) :: which
            real(kind=8) :: tol
            real(kind=8) :: resid(n)
            real(kind=8) :: v(ldv,ncv)
            integer :: iparam(11)
            integer :: ipntr(14)
            real(kind=8) :: workd(3*n)
            real(kind=8) :: workl(lworkl)
            integer :: info
          end subroutine dneupd
        end interface
