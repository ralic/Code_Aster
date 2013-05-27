        interface
          subroutine nmassv(typvez,modelz,lischa,mate,carele,compor,&
     &numedd,instam,instap,resoco,resocu,sddyna,sdtime,valinc,comref,&
     &measse,vecelz,vecasz)
            character(*) :: typvez
            character(*) :: modelz
            character(len=19) :: lischa
            character(len=24) :: mate
            character(len=24) :: carele
            character(len=24) :: compor
            character(len=24) :: numedd
            real(kind=8) :: instam
            real(kind=8) :: instap
            character(len=24) :: resoco
            character(len=24) :: resocu
            character(len=19) :: sddyna
            character(len=24) :: sdtime
            character(len=19) :: valinc(*)
            character(len=24) :: comref
            character(len=19) :: measse(*)
            character(*) :: vecelz
            character(*) :: vecasz
          end subroutine nmassv
        end interface
