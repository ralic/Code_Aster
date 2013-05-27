        interface
          subroutine meca01(optio0,nbordr,jordr,nchar,jcha,kcha,ctyp,&
     &tbgrca,resuco,resuc1,leres1,noma,modele,ligrmo,mate,cara,chvarc,&
     &codret)
            character(*) :: optio0
            integer :: nbordr
            integer :: jordr
            integer :: nchar
            integer :: jcha
            character(len=19) :: kcha
            character(len=4) :: ctyp
            real(kind=8) :: tbgrca(3)
            character(len=8) :: resuco
            character(len=8) :: resuc1
            character(len=19) :: leres1
            character(len=8) :: noma
            character(len=8) :: modele
            character(len=24) :: ligrmo
            character(len=24) :: mate
            character(len=8) :: cara
            character(len=19) :: chvarc
            integer :: codret
          end subroutine meca01
        end interface
