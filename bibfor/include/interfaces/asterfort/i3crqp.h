        interface
          subroutine i3crqp(epsi,seuil,s,x,y,cr,iret)
            real(kind=8) :: epsi
            real(kind=8) :: seuil
            real(kind=8) :: s(3,*)
            real(kind=8) :: x
            real(kind=8) :: y
            real(kind=8) :: cr(*)
            integer :: iret
          end subroutine i3crqp
        end interface
