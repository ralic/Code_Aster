        interface
          subroutine extchn(nchmno,nnoeud,numnd,ncmp,nbn,nbc,indic,&
     &nsschn,mcf,iocc)
            character(len=19) :: nchmno
            character(len=8) :: nnoeud(*)
            integer :: numnd(*)
            character(len=8) :: ncmp(*)
            integer :: nbn
            integer :: nbc
            character(len=6) :: indic
            character(len=19) :: nsschn
            character(*) :: mcf
            integer :: iocc
          end subroutine extchn
        end interface
