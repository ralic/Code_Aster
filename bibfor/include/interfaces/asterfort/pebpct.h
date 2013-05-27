        interface
          subroutine pebpct(modele,nbma,lma,cham,nomcmp,dim,bfix,borne&
     &,norme,borpct,voltot)
            integer :: dim
            character(len=8) :: modele
            integer :: nbma
            character(len=24) :: lma
            character(len=19) :: cham
            character(len=8) :: nomcmp
            integer :: bfix
            real(kind=8) :: borne(2)
            character(len=8) :: norme
            real(kind=8) :: borpct(dim)
            real(kind=8) :: voltot
          end subroutine pebpct
        end interface
