        interface
          subroutine usubis(type,para,crit,epsi,x1,x2,x,iret)
            character(*) :: type
            real(kind=8) :: para(*)
            character(*) :: crit
            real(kind=8) :: epsi
            real(kind=8) :: x1
            real(kind=8) :: x2
            real(kind=8) :: x
            integer :: iret
          end subroutine usubis
        end interface
