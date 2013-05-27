        interface
          subroutine resthe(ligrel,evol,chtemm,chtemp,chflum,chflup,&
     &mate,valthe,insold,inst,resu,niveau,ifm,niv,ma,cartef,nomgdf,&
     &carteh,nomgdh,cartet,nomgdt,cartes,nomgds,chgeom,chsour,psourc,&
     &iaux)
            character(len=24) :: ligrel
            logical :: evol
            character(len=24) :: chtemm
            character(len=24) :: chtemp
            character(len=24) :: chflum
            character(len=24) :: chflup
            character(len=24) :: mate
            real(kind=8) :: valthe
            real(kind=8) :: insold
            real(kind=8) :: inst
            character(len=24) :: resu
            integer :: niveau
            integer :: ifm
            integer :: niv
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
            integer :: iaux
          end subroutine resthe
        end interface
