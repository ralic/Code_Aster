        interface
          subroutine vpgskp(nbeq,nconv,vect,alpha,lmatb,typeps,vaux,&
     &ddlexc,delta)
            integer :: nconv
            integer :: nbeq
            real(kind=8) :: vect(nbeq,nconv)
            real(kind=8) :: alpha
            integer :: lmatb
            integer :: typeps
            real(kind=8) :: vaux(nbeq)
            integer :: ddlexc(nbeq)
            real(kind=8) :: delta(nconv)
          end subroutine vpgskp
        end interface
