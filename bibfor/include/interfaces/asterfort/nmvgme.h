        interface
          subroutine nmvgme(modele,ligrel,carele,charge,icha,instan,&
     &resufv,depmoi,depdel,vites)
            character(*) :: modele
            character(*) :: ligrel
            character(*) :: carele
            character(*) :: charge
            integer :: icha
            real(kind=8) :: instan
            character(*) :: resufv(1)
            character(*) :: depmoi
            character(*) :: depdel
            character(*) :: vites
          end subroutine nmvgme
        end interface
