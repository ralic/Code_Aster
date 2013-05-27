        interface
          subroutine geolis(modgen,sst1,sst2,intf1,intf2,geom1,geom2,&
     &limail,nmga1)
            character(len=8) :: modgen
            character(len=8) :: sst1
            character(len=8) :: sst2
            character(len=8) :: intf1
            character(len=8) :: intf2
            character(len=24) :: geom1
            character(len=24) :: geom2
            character(*) :: limail
            integer :: nmga1
          end subroutine geolis
        end interface
