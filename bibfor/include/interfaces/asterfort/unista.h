        interface
          subroutine unista(h,ldh,v,ldv,ddlsta,n,vectp,csta,beta,etat,&
     &ldynfa,ddlexc,redem)
            integer :: n
            integer :: ldv
            integer :: ldh
            real(kind=8) :: h(ldh,ldh)
            real(kind=8) :: v(ldv,ldh)
            integer :: ddlsta(n)
            real(kind=8) :: vectp(ldv)
            real(kind=8) :: csta
            real(kind=8) :: beta
            integer :: etat
            integer :: ldynfa
            integer :: ddlexc(n)
            integer :: redem
          end subroutine unista
        end interface
