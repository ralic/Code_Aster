        interface
          subroutine nmcrsu(sddisc,lisins,parcri,limpex,lctcd,solveu,&
     &defico)
            character(len=19) :: sddisc
            character(len=19) :: lisins
            real(kind=8) :: parcri(*)
            logical :: limpex
            logical :: lctcd
            character(len=19) :: solveu
            character(len=24) :: defico
          end subroutine nmcrsu
        end interface
