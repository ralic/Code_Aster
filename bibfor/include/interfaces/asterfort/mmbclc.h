        interface
          subroutine mmbclc(noma,nomo,numedd,iterat,numins,sddisc,&
     &sddyna,sdimpr,defico,resoco,valinc,solalg,sdtime,sdstat,mmcvca)
            character(len=8) :: noma
            character(len=8) :: nomo
            character(len=24) :: numedd
            integer :: iterat
            integer :: numins
            character(len=19) :: sddisc
            character(len=19) :: sddyna
            character(len=24) :: sdimpr
            character(len=24) :: defico
            character(len=24) :: resoco
            character(len=19) :: valinc(*)
            character(len=19) :: solalg(*)
            character(len=24) :: sdtime
            character(len=24) :: sdstat
            logical :: mmcvca
          end subroutine mmbclc
        end interface
