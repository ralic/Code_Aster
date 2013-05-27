        interface
          subroutine nmdocc(compor,modele,nbmo1,moclef,nomcmp,ncmpma,&
     &meca,nomcmd)
            character(len=19) :: compor
            character(len=24) :: modele
            integer :: nbmo1
            character(len=16) :: moclef(2)
            character(len=8) :: nomcmp(*)
            integer :: ncmpma
            logical :: meca
            character(len=16) :: nomcmd
          end subroutine nmdocc
        end interface
