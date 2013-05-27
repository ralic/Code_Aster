        interface
          subroutine cfmxsd(noma,nomo,numedd,fonact,sddyna,defico,&
     &resoco,ligrcf,ligrxf)
            character(len=8) :: noma
            character(len=8) :: nomo
            character(len=24) :: numedd
            integer :: fonact(*)
            character(len=19) :: sddyna
            character(len=24) :: defico
            character(len=24) :: resoco
            character(len=19) :: ligrcf
            character(len=19) :: ligrxf
          end subroutine cfmxsd
        end interface
