        interface
          subroutine ndgrot(sddyna,valinc,solalg,deldet,theta1,theta2,&
     &iran)
            character(len=19) :: sddyna
            character(len=19) :: valinc(*)
            character(len=19) :: solalg(*)
            real(kind=8) :: deldet(3)
            real(kind=8) :: theta1(3)
            real(kind=8) :: theta2(3)
            integer :: iran(3)
          end subroutine ndgrot
        end interface
