        interface
          subroutine jjimhd(idfic,inat,crnom,ngrp,kattr,iadmi,genri,&
     &typei,lt,lonoi)
            integer :: idfic
            integer :: inat
            character(*) :: crnom
            character(*) :: ngrp
            character(len=24) :: kattr(*)
            integer :: iadmi
            character(*) :: genri
            character(*) :: typei
            integer :: lt
            integer :: lonoi
          end subroutine jjimhd
        end interface
