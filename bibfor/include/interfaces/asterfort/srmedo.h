        interface
          subroutine srmedo(modele,mate,cara,kcha,ncha,ctyp,result,&
     &nuord,nbordr,base,npass,ligrel)
            character(len=8) :: modele
            character(len=24) :: mate
            character(len=8) :: cara
            character(len=19) :: kcha
            integer :: ncha
            character(len=4) :: ctyp
            character(len=8) :: result
            integer :: nuord
            integer :: nbordr
            character(len=1) :: base
            integer :: npass
            character(len=24) :: ligrel
          end subroutine srmedo
        end interface
