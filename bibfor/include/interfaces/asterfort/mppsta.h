        interface
          subroutine mppsta(h,ldh,v,ldv,ddlsta,n,vectt,ddlexc,indico,&
     &proj)
            integer :: n
            integer :: ldv
            integer :: ldh
            real(kind=8) :: h(ldh,ldh)
            real(kind=8) :: v(ldv,ldh)
            integer :: ddlsta(n)
            real(kind=8) :: vectt(ldv)
            integer :: ddlexc(n)
            integer :: indico
            integer :: proj
          end subroutine mppsta
        end interface
