        interface
          subroutine dnaup2(ido,bmat,n,which,nev,np,tol,resid,ishift,&
     &mxiter,v,ldv,h,ldh,ritzr,ritzi,bounds,q,ldq,workl,ipntr,workd,info&
     &,neqact,alpha)
            integer :: ldq
            integer :: ldh
            integer :: ldv
            integer :: np
            integer :: nev
            integer :: n
            integer :: ido
            character(len=1) :: bmat
            character(len=2) :: which
            real(kind=8) :: tol
            real(kind=8) :: resid(n)
            integer :: ishift
            integer :: mxiter
            real(kind=8) :: v(ldv,nev+np)
            real(kind=8) :: h(ldh,nev+np)
            real(kind=8) :: ritzr(nev+np)
            real(kind=8) :: ritzi(nev+np)
            real(kind=8) :: bounds(nev+np)
            real(kind=8) :: q(ldq,nev+np)
            real(kind=8) :: workl((nev+np)*(nev+np+3))
            integer :: ipntr(13)
            real(kind=8) :: workd(3*n)
            integer :: info
            integer :: neqact
            real(kind=8) :: alpha
          end subroutine dnaup2
        end interface
