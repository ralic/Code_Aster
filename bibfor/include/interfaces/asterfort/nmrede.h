        interface
          subroutine nmrede(numedd,sdnume,fonact,sddyna,matass,veasse,&
     &neq,foiner,cnfext,cnfint,vchar,ichar)
            character(len=24) :: numedd
            character(len=19) :: sdnume
            integer :: fonact(*)
            character(len=19) :: sddyna
            character(len=19) :: matass
            character(len=19) :: veasse(*)
            integer :: neq
            character(len=19) :: foiner
            character(len=19) :: cnfext
            character(len=19) :: cnfint
            real(kind=8) :: vchar
            integer :: ichar
          end subroutine nmrede
        end interface
