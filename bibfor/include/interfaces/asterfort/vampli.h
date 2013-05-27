        interface
          subroutine vampli(vwork,tdisp,liste,nbt,nbordr,numini,nbp,&
     &tspaq,nomopt,cxsr)
            integer :: nbp
            integer :: tdisp
            real(kind=8) :: vwork(tdisp)
            integer :: liste(nbp)
            integer :: nbt
            integer :: nbordr
            integer :: numini
            integer :: tspaq
            character(len=16) :: nomopt
            character(len=19) :: cxsr
          end subroutine vampli
        end interface
