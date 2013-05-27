        interface
          subroutine usunew(type,para,crit,epsi,x1,x2,resu,iret)
            character(*) :: type
            real(kind=8) :: para(*)
            character(*) :: crit
            real(kind=8) :: epsi
            real(kind=8) :: x1
            real(kind=8) :: x2
            real(kind=8) :: resu
            integer :: iret
          end subroutine usunew
        end interface
