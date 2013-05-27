        interface
          subroutine mamodg(model,stolci,nomres,itxsto,itysto,itzsto,&
     &iprsto,iadirg,nbmo,max,may,maz,nbloc)
            character(len=2) :: model
            character(len=19) :: stolci
            character(len=8) :: nomres
            integer :: itxsto
            integer :: itysto
            integer :: itzsto
            integer :: iprsto
            integer :: iadirg
            integer :: nbmo
            character(len=19) :: max
            character(len=19) :: may
            character(len=19) :: maz
            integer :: nbloc
          end subroutine mamodg
        end interface
