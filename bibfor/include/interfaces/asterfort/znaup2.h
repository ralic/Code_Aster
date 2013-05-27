        interface
          subroutine znaup2(ido,bmat,n,which,nev,np,tol,resid,ishift,&
     &mxiter,v,ldv,h,ldh,ritz,bounds,q,ldq,workl,ipntr,workd,rwork,info,&
     &neqact,alpha)
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
            complex(kind=8) :: resid(n)
            integer :: ishift
            integer :: mxiter
            complex(kind=8) :: v(ldv,nev+np)
            complex(kind=8) :: h(ldh,nev+np)
            complex(kind=8) :: ritz(nev+np)
            complex(kind=8) :: bounds(nev+np)
            complex(kind=8) :: q(ldq,nev+np)
            complex(kind=8) :: workl((nev+np)*(nev+np+3))
            integer :: ipntr(13)
            complex(kind=8) :: workd(3*n)
            real(kind=8) :: rwork(nev+np)
            integer :: info
            integer :: neqact
            real(kind=8) :: alpha
          end subroutine znaup2
        end interface
