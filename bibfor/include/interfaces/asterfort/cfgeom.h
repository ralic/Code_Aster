        interface
          subroutine cfgeom(reageo,iterat,noma,sdtime,sdstat,defico,&
     &resoco,depplu)
            logical :: reageo
            integer :: iterat
            character(len=8) :: noma
            character(len=24) :: sdtime
            character(len=24) :: sdstat
            character(len=24) :: defico
            character(len=24) :: resoco
            character(len=19) :: depplu
          end subroutine cfgeom
        end interface
