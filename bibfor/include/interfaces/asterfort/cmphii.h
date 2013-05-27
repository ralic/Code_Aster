        interface
          subroutine cmphii(ck,cm,ndim,nbmod,niter,xcrit,ceigen,cmod,&
     &ndimax,cmat1,cmat2,cvec,ific)
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
            complex(kind=8) :: cvec(ndim)
            integer :: ific
          end subroutine cmphii
        end interface
