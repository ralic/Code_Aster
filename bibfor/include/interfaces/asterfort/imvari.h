        interface
          subroutine imvari(moclef,iocc,ncomel,lcomel,comcod,nbvari,&
     &tavari)
            character(len=16) :: moclef
            integer :: iocc
            integer :: ncomel
            character(len=16) :: lcomel(5)
            character(len=16) :: comcod
            integer :: nbvari
            character(len=8) :: tavari
          end subroutine imvari
        end interface
