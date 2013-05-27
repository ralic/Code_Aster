        interface
          subroutine sleelt(iunv,maxnod,nbtyma,indic,permut,nbmail,&
     &mint,mant,datset,inum)
            integer :: nbtyma
            integer :: maxnod
            integer :: iunv
            integer :: indic(nbtyma)
            integer :: permut(maxnod,nbtyma)
            integer :: nbmail(nbtyma)
            integer :: mint(nbtyma)
            integer :: mant(nbtyma)
            integer :: datset
            integer :: inum
          end subroutine sleelt
        end interface
