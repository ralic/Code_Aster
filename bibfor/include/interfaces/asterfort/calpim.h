        interface
          subroutine calpim(graexc,excmod,napexc,nbmode,tymmec,mtrmas,&
     &numer,nbddl,noexit,cpexit,nvasex,vecass)
            character(len=16) :: graexc
            character(len=4) :: excmod
            integer :: napexc
            integer :: nbmode
            character(len=8) :: tymmec
            character(len=8) :: mtrmas
            character(len=8) :: numer
            integer :: nbddl
            character(len=8) :: noexit(*)
            character(len=8) :: cpexit(*)
            integer :: nvasex
            character(len=8) :: vecass(*)
          end subroutine calpim
        end interface
