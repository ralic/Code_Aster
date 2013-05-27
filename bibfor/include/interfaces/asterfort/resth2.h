        interface
          subroutine resth2(modele,ligrel,lchar,nchar,ma,cartef,nomgdf&
     &,carteh,nomgdh,cartet,nomgdt,cartes,nomgds,chgeom,chsour,psourc)
            character(len=8) :: modele
            character(len=24) :: ligrel
            character(len=8) :: lchar(1)
            integer :: nchar
            character(len=8) :: ma
            character(len=19) :: cartef
            character(len=19) :: nomgdf
            character(len=19) :: carteh
            character(len=19) :: nomgdh
            character(len=19) :: cartet
            character(len=19) :: nomgdt
            character(len=19) :: cartes
            character(len=19) :: nomgds
            character(len=24) :: chgeom
            character(len=24) :: chsour
            character(len=8) :: psourc
          end subroutine resth2
        end interface
