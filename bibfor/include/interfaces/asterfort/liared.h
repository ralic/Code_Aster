        interface
          subroutine liared(nomres,fmli,iblo,liamod,nlilia,ncolia,&
     &promod,nlipro,ncopro,taille,indcol,nbcol)
            character(len=8) :: nomres
            character(len=24) :: fmli
            integer :: iblo
            character(len=24) :: liamod
            integer :: nlilia
            integer :: ncolia
            character(len=24) :: promod
            integer :: nlipro
            integer :: ncopro
            integer :: taille(2)
            character(len=24) :: indcol
            integer :: nbcol
          end subroutine liared
        end interface
