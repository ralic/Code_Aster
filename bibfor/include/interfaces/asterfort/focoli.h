        interface
          subroutine focoli(ipt,coli,interp,x,y,rvar,resu,ier)
            integer :: ipt
            character(len=1) :: coli
            character(len=24) :: interp
            real(kind=8) :: x(*)
            real(kind=8) :: y(*)
            real(kind=8) :: rvar
            real(kind=8) :: resu
            integer :: ier
          end subroutine focoli
        end interface
