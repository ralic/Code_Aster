        interface
          subroutine ecrtes(nomsd,titre,nomgds,numor,fitype,nbcmp,ityp&
     &,entete,lcmp)
            character(*) :: nomsd
            character(*) :: titre
            character(*) :: nomgds
            integer :: numor
            character(*) :: fitype
            integer :: nbcmp
            integer :: ityp
            character(len=80) :: entete(10)
            logical :: lcmp
          end subroutine ecrtes
        end interface
