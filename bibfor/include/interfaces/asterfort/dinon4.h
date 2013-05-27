        interface
          subroutine dinon4(neq,ul,dul,utl,nno,nbcomp,varimo,raide,&
     &nbpar,param,okdire,varipl)
            integer :: nbpar
            integer :: nbcomp
            integer :: neq
            real(kind=8) :: ul(neq)
            real(kind=8) :: dul(neq)
            real(kind=8) :: utl(neq)
            integer :: nno
            real(kind=8) :: varimo(nbcomp*1)
            real(kind=8) :: raide(nbcomp)
            real(kind=8) :: param(6,nbpar)
            logical :: okdire(6)
            real(kind=8) :: varipl(nbcomp*1)
          end subroutine dinon4
        end interface
