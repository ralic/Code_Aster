        interface
          subroutine nminvc(modelz,mate,carele,compor,carcri,sdtime,&
     &sddisc,sddyna,valinc,solalg,lischa,comref,resoco,resocu,numedd,&
     &fonact,parcon,veelem,veasse,measse)
            character(*) :: modelz
            character(len=24) :: mate
            character(len=24) :: carele
            character(len=24) :: compor
            character(len=24) :: carcri
            character(len=24) :: sdtime
            character(len=19) :: sddisc
            character(len=19) :: sddyna
            character(len=19) :: valinc(*)
            character(len=19) :: solalg(*)
            character(len=19) :: lischa
            character(len=24) :: comref
            character(len=24) :: resoco
            character(len=24) :: resocu
            character(len=24) :: numedd
            integer :: fonact(*)
            real(kind=8) :: parcon(8)
            character(len=19) :: veelem(*)
            character(len=19) :: veasse(*)
            character(len=19) :: measse(*)
          end subroutine nminvc
        end interface
