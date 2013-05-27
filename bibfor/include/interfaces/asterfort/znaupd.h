        interface
          subroutine znaupd(ido,bmat,n,which,nev,tol,resid,ncv,v,ldv,&
     &iparam,ipntr,workd,workl,lworkl,rwork,info,neqact,alpha)
            integer :: lworkl
            integer :: ldv
            integer :: ncv
            integer :: n
            integer :: ido
            character(len=1) :: bmat
            character(len=2) :: which
            integer :: nev
            real(kind=8) :: tol
            complex(kind=8) :: resid(n)
            complex(kind=8) :: v(ldv,ncv)
            integer :: iparam(11)
            integer :: ipntr(14)
            complex(kind=8) :: workd(3*n)
            complex(kind=8) :: workl(lworkl)
            real(kind=8) :: rwork(ncv)
            integer :: info
            integer :: neqact
            real(kind=8) :: alpha
          end subroutine znaupd
        end interface
