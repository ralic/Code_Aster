        interface
          subroutine nmresd(fonact,sddyna,sdstat,sdtime,solveu,numedd,&
     &instan,maprec,matass,cndonn,cnpilo,cncine,solalg,rescvg)
            integer :: fonact(*)
            character(len=19) :: sddyna
            character(len=24) :: sdstat
            character(len=24) :: sdtime
            character(len=19) :: solveu
            character(len=24) :: numedd
            real(kind=8) :: instan
            character(len=19) :: maprec
            character(len=19) :: matass
            character(len=19) :: cndonn
            character(len=19) :: cnpilo
            character(len=19) :: cncine
            character(len=19) :: solalg(*)
            integer :: rescvg
          end subroutine nmresd
        end interface
