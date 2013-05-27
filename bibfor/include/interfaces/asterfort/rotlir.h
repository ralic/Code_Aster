        interface
          subroutine rotlir(nomres,sst1,intf1,lino1,codret,indin1,&
     &tramo1,ddla1,nbeq1,imast,numlia)
            character(len=8) :: nomres
            character(len=8) :: sst1
            character(len=8) :: intf1
            character(len=24) :: lino1
            integer :: codret
            character(len=24) :: indin1
            character(len=24) :: tramo1
            integer :: ddla1
            integer :: nbeq1
            integer :: imast
            integer :: numlia
          end subroutine rotlir
        end interface
