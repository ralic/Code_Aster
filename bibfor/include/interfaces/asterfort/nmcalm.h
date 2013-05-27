        interface
          subroutine nmcalm(typmat,modelz,lischa,mate,carele,compor,&
     &instam,instap,carcri,valinc,solalg,optmaz,base,meelem,defico,&
     &resoco,matele)
            character(len=6) :: typmat
            character(*) :: modelz
            character(len=19) :: lischa
            character(*) :: mate
            character(*) :: carele
            character(len=24) :: compor
            real(kind=8) :: instam
            real(kind=8) :: instap
            character(len=24) :: carcri
            character(len=19) :: valinc(*)
            character(len=19) :: solalg(*)
            character(*) :: optmaz
            character(len=1) :: base
            character(len=19) :: meelem(*)
            character(len=24) :: defico
            character(len=24) :: resoco
            character(len=19) :: matele
          end subroutine nmcalm
        end interface
