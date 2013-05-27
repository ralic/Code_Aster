        interface
          subroutine nmdoki(moclef,modele,comp,k,dimaki,nbkit,nomkit,&
     &nbnvi,ncomel,lcomel,numlc,nbvari)
            integer :: dimaki
            character(len=16) :: moclef
            character(*) :: modele
            character(len=16) :: comp
            integer :: k
            integer :: nbkit
            character(len=16) :: nomkit(dimaki)
            integer :: nbnvi(*)
            integer :: ncomel
            character(len=16) :: lcomel(*)
            integer :: numlc
            integer :: nbvari
          end subroutine nmdoki
        end interface
