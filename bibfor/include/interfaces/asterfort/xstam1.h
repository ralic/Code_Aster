        interface
          subroutine xstam1(nomo,noma,nbma,nmafis,mafis,stano,mafon,&
     &maen1,maen2,maen3,nmafon,nmaen1,nmaen2,nmaen3)
            integer :: nmafis
            integer :: nbma
            character(len=8) :: nomo
            character(len=8) :: noma
            integer :: mafis(nmafis)
            integer :: stano(*)
            integer :: mafon(nmafis)
            integer :: maen1(nbma)
            integer :: maen2(nbma)
            integer :: maen3(nbma)
            integer :: nmafon
            integer :: nmaen1
            integer :: nmaen2
            integer :: nmaen3
          end subroutine xstam1
        end interface
