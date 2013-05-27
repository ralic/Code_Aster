        interface
          subroutine nmxvec(modelz,mate,carele,compor,carcri,sdtime,&
     &sddisc,sddyna,numins,valinc,solalg,lischa,comref,resoco,resocu,&
     &numedd,parcon,veelem,veasse,measse,nbvect,ltypve,lcalve,loptve,&
     &lassve)
            character(*) :: modelz
            character(len=24) :: mate
            character(len=24) :: carele
            character(len=24) :: compor
            character(len=24) :: carcri
            character(len=24) :: sdtime
            character(len=19) :: sddisc
            character(len=19) :: sddyna
            integer :: numins
            character(len=19) :: valinc(*)
            character(len=19) :: solalg(*)
            character(len=19) :: lischa
            character(len=24) :: comref
            character(len=24) :: resoco
            character(len=24) :: resocu
            character(len=24) :: numedd
            real(kind=8) :: parcon(*)
            character(len=19) :: veelem(*)
            character(len=19) :: veasse(*)
            character(len=19) :: measse(*)
            integer :: nbvect
            character(len=6) :: ltypve(20)
            logical :: lcalve(20)
            character(len=16) :: loptve(20)
            logical :: lassve(20)
          end subroutine nmxvec
        end interface
