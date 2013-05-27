        interface
          subroutine xpoini(maxfem,modele,malini,modvis,licham,resuco,&
     &resux,prefno,nogrfi)
            character(len=8) :: maxfem
            character(len=8) :: modele
            character(len=8) :: malini
            character(len=8) :: modvis
            character(len=24) :: licham
            character(len=8) :: resuco
            character(len=8) :: resux
            character(len=2) :: prefno(4)
            character(len=24) :: nogrfi
          end subroutine xpoini
        end interface
