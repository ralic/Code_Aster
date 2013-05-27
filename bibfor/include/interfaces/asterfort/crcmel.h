        interface
          subroutine crcmel(nbmo1,moclef,compor,ces2,modele,ncmpma,&
     &nomcmp,nt)
            integer :: ncmpma
            integer :: nbmo1
            character(len=16) :: moclef(2)
            character(len=19) :: compor
            character(len=19) :: ces2
            character(len=24) :: modele
            character(len=8) :: nomcmp(ncmpma)
            integer :: nt
          end subroutine crcmel
        end interface
