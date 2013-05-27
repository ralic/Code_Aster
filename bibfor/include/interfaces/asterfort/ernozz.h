        interface
          subroutine ernozz(modele,sigma,chmat,signo,chvarc,option,&
     &ligrel,iordr,resuco,resuc1,champ)
            character(*) :: modele
            character(*) :: sigma
            character(*) :: chmat
            character(*) :: signo
            character(len=19) :: chvarc
            character(*) :: option
            character(*) :: ligrel
            integer :: iordr
            character(*) :: resuco
            character(len=19) :: resuc1
            character(*) :: champ
          end subroutine ernozz
        end interface
