        interface
          subroutine nmcalv(typvec,modelz,lischa,mate,carele,compor,&
     &carcri,numedd,comref,sdtime,parcon,instam,instap,valinc,solalg,&
     &sddyna,option,vecele)
            character(len=6) :: typvec
            character(*) :: modelz
            character(len=19) :: lischa
            character(len=24) :: mate
            character(len=24) :: carele
            character(len=24) :: compor
            character(len=24) :: carcri
            character(len=24) :: numedd
            character(len=24) :: comref
            character(len=24) :: sdtime
            real(kind=8) :: parcon(*)
            real(kind=8) :: instam
            real(kind=8) :: instap
            character(len=19) :: valinc(*)
            character(len=19) :: solalg(*)
            character(len=19) :: sddyna
            character(len=16) :: option
            character(len=19) :: vecele
          end subroutine nmcalv
        end interface
