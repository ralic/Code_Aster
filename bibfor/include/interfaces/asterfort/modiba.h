        interface
          subroutine modiba(nomres,basemo,basefl,numvit,newres,itypfl,&
     &imasse,nuor,nbnuor,numo,nbmfl)
            character(len=8) :: nomres
            character(len=8) :: basemo
            character(len=19) :: basefl
            integer :: numvit
            logical :: newres
            integer :: itypfl
            integer :: imasse
            integer :: nuor(*)
            integer :: nbnuor
            integer :: numo(*)
            integer :: nbmfl
          end subroutine modiba
        end interface
