        interface
          subroutine cmphdi(ck,cm,ndim,nbmod,niter,xcrit,ceigen,cmod,&
     &ndimax,cmat1,cmat2,cvect,cvect1,alpha,beta,lambd1,lambd2,interv,&
     &ific)
            integer :: ndimax
            integer :: nbmod
            integer :: ndim
            complex(kind=8) :: ck(*)
            complex(kind=8) :: cm(*)
            integer :: niter
            real(kind=8) :: xcrit
            complex(kind=8) :: ceigen(nbmod)
            complex(kind=8) :: cmod(ndimax,nbmod)
            complex(kind=8) :: cmat1(*)
            complex(kind=8) :: cmat2(ndim,ndim)
            complex(kind=8) :: cvect(ndim)
            complex(kind=8) :: cvect1(ndim)
            real(kind=8) :: alpha(ndim+1)
            real(kind=8) :: beta(ndim+1)
            real(kind=8) :: lambd1
            real(kind=8) :: lambd2
            real(kind=8) :: interv
            integer :: ific
          end subroutine cmphdi
        end interface
