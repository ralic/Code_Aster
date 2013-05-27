        interface
          subroutine exiscp(nomcmp,char,modele,nbnd,typend,nomnd,numnd&
     &,resu)
            character(len=8) :: nomcmp
            character(len=8) :: char
            character(len=8) :: modele
            integer :: nbnd
            character(len=3) :: typend
            character(len=8) :: nomnd(*)
            integer :: numnd(*)
            integer :: resu(*)
          end subroutine exiscp
        end interface
