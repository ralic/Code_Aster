        interface
          subroutine projca(tablca,lirela,nmabet,nbmabe,mailla,nbnobe,&
     &nunobe,icabl,nbnoca,xnoca,ynoca,znoca)
            character(len=19) :: tablca
            character(len=19) :: lirela
            character(len=24) :: nmabet
            integer :: nbmabe
            character(len=8) :: mailla
            integer :: nbnobe
            character(len=19) :: nunobe
            integer :: icabl
            integer :: nbnoca(*)
            character(len=19) :: xnoca
            character(len=19) :: ynoca
            character(len=19) :: znoca
          end subroutine projca
        end interface
