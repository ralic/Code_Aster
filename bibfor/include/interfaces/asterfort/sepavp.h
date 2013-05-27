        interface
          subroutine sepavp(ck,cm,cmat,ndim,alpha,beta,nbmod,lambd1,&
     &lambd2,interv,ific)
            integer :: ndim
            complex(kind=8) :: ck(*)
            complex(kind=8) :: cm(*)
            complex(kind=8) :: cmat(*)
            real(kind=8) :: alpha(ndim+1)
            real(kind=8) :: beta(ndim+1)
            integer :: nbmod
            real(kind=8) :: lambd1
            real(kind=8) :: lambd2
            real(kind=8) :: interv
            integer :: ific
          end subroutine sepavp
        end interface
