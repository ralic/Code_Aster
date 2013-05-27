        interface
          subroutine thcalr(newcal,tysd,knum,kcha,resuco,resuc1,nbordr&
     &,modele,mate,cara,nchar,ctyp)
            logical :: newcal
            character(len=16) :: tysd
            character(len=19) :: knum
            character(len=19) :: kcha
            character(len=8) :: resuco
            character(len=8) :: resuc1
            integer :: nbordr
            character(len=8) :: modele
            character(len=24) :: mate
            character(len=8) :: cara
            integer :: nchar
            character(len=4) :: ctyp
          end subroutine thcalr
        end interface
