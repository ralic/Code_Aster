        interface
          subroutine zneupd(rvec,howmny,select,d,z,ldz,sigma,workev,&
     &bmat,n,which,nev,tol,resid,ncv,v,ldv,iparam,ipntr,workd,workl,&
     &lworkl,rwork,info)
            integer :: lworkl
            integer :: ldv
            integer :: ncv
            integer :: n
            integer :: ldz
            logical :: rvec
            character(len=1) :: howmny
            logical :: select(*)
            complex(kind=8) :: d(*)
            complex(kind=8) :: z(ldz,*)
            complex(kind=8) :: sigma
            complex(kind=8) :: workev(2*ncv)
            character(len=1) :: bmat
            character(len=2) :: which
            integer :: nev
            real(kind=8) :: tol
            complex(kind=8) :: resid(*)
            complex(kind=8) :: v(ldv,*)
            integer :: iparam(11)
            integer :: ipntr(14)
            complex(kind=8) :: workd(3*n)
            complex(kind=8) :: workl(lworkl)
            real(kind=8) :: rwork(*)
            integer :: info
          end subroutine zneupd
        end interface
