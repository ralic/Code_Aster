        interface
          subroutine i3nwt2(epsi,seuil,maxitr,fk,x,iret)
            real(kind=8) :: epsi
            real(kind=8) :: seuil
            integer :: maxitr
            real(kind=8) :: fk(4,*)
            real(kind=8) :: x(*)
            integer :: iret
          end subroutine i3nwt2
        end interface
