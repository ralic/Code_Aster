        interface
          subroutine gma110(nbgr,exclu,nbgrut,mailla,nomsst,nbtgrm,&
     &nomres,nbincr,tabsgr,tabsst,tabgma,tabnom)
            integer :: nbgr
            character(len=8) :: exclu
            integer :: nbgrut
            character(len=8) :: mailla
            character(len=8) :: nomsst
            integer :: nbtgrm
            character(len=8) :: nomres
            integer :: nbincr
            character(len=24) :: tabsgr(*)
            character(len=8) :: tabsst(*)
            character(len=24) :: tabgma(*)
            character(len=24) :: tabnom(*)
          end subroutine gma110
        end interface
