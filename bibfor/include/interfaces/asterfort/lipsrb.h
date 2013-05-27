        interface
          subroutine lipsrb(nomres,matprj,sst1,sst2,intf1,intf2,lino1,&
     &lino2,indin1,indin2,ddlmas,ddlsla,nbmoma,nbmosl,imast,tramod)
            character(len=8) :: nomres
            character(len=8) :: matprj
            character(len=8) :: sst1
            character(len=8) :: sst2
            character(len=8) :: intf1
            character(len=8) :: intf2
            character(len=24) :: lino1
            character(len=24) :: lino2
            character(len=24) :: indin1
            character(len=24) :: indin2
            integer :: ddlmas
            integer :: ddlsla
            integer :: nbmoma
            integer :: nbmosl
            integer :: imast
            character(len=24) :: tramod
          end subroutine lipsrb
        end interface
