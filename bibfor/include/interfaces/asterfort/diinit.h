        interface
          subroutine diinit(noma,nomo,result,mate,carele,fonact,sddyna&
     &,parcri,instin,sdieto,solveu,defico,sddisc,sdobse,sdsuiv)
            character(len=8) :: noma
            character(len=8) :: nomo
            character(len=8) :: result
            character(len=24) :: mate
            character(len=24) :: carele
            integer :: fonact(*)
            character(len=19) :: sddyna
            real(kind=8) :: parcri(*)
            real(kind=8) :: instin
            character(len=24) :: sdieto
            character(len=19) :: solveu
            character(len=24) :: defico
            character(len=19) :: sddisc
            character(len=19) :: sdobse
            character(len=24) :: sdsuiv
          end subroutine diinit
        end interface
