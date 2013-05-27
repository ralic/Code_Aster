        interface
          subroutine genecy(cmod1,cmod2,neq,lmat,para,nbsec,beta1,&
     &beta2,ctrav)
            integer :: neq
            complex(kind=8) :: cmod1(neq)
            complex(kind=8) :: cmod2(neq)
            integer :: lmat
            real(kind=8) :: para(2)
            integer :: nbsec
            real(kind=8) :: beta1
            real(kind=8) :: beta2
            complex(kind=8) :: ctrav(neq)
          end subroutine genecy
        end interface
