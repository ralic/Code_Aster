        interface
          subroutine nmxmat(modelz,mate,carele,compor,carcri,sddisc,&
     &sddyna,fonact,numins,iterat,valinc,solalg,lischa,comref,defico,&
     &resoco,solveu,numedd,numfix,sdstat,sdtime,nbmatr,ltypma,loptme,&
     &loptma,lcalme,lassme,lcfint,meelem,measse,veelem,ldccvg,codere)
            character(*) :: modelz
            character(*) :: mate
            character(len=24) :: carele
            character(len=24) :: compor
            character(len=24) :: carcri
            character(len=19) :: sddisc
            character(len=19) :: sddyna
            integer :: fonact(*)
            integer :: numins
            integer :: iterat
            character(len=19) :: valinc(*)
            character(len=19) :: solalg(*)
            character(len=19) :: lischa
            character(len=24) :: comref
            character(len=24) :: defico
            character(len=24) :: resoco
            character(len=19) :: solveu
            character(len=24) :: numedd
            character(len=24) :: numfix
            character(len=24) :: sdstat
            character(len=24) :: sdtime
            integer :: nbmatr
            character(len=6) :: ltypma(20)
            character(len=16) :: loptme(20)
            character(len=16) :: loptma(20)
            logical :: lcalme(20)
            logical :: lassme(20)
            logical :: lcfint
            character(len=19) :: meelem(*)
            character(len=19) :: measse(*)
            character(len=19) :: veelem(*)
            integer :: ldccvg
            character(len=24) :: codere
          end subroutine nmxmat
        end interface
