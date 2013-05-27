        interface
          subroutine dinon2(neq,ul,dul,utl,nno,nbcomp,varimo,raide,&
     &nbpar,param,okdire,varipl,dt)
            integer :: nbpar
            integer :: nbcomp
            integer :: neq
            real(kind=8) :: ul(neq)
            real(kind=8) :: dul(neq)
            real(kind=8) :: utl(neq)
            integer :: nno
            real(kind=8) :: varimo(nbcomp*2)
            real(kind=8) :: raide(nbcomp)
            real(kind=8) :: param(6,nbpar)
            logical :: okdire(6)
            real(kind=8) :: varipl(nbcomp*2)
            real(kind=8) :: dt
          end subroutine dinon2
        end interface
