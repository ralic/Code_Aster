        interface
          subroutine nmdepr(modele,ligrel,carele,charge,icha,instan,&
     &resufv)
            character(*) :: modele
            character(*) :: ligrel
            character(*) :: carele
            character(*) :: charge
            integer :: icha
            real(kind=8) :: instan
            character(*) :: resufv(3)
          end subroutine nmdepr
        end interface
