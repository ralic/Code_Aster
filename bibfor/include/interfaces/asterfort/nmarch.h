        interface
          subroutine nmarch(result,numins,modele,mate,carele,fonact,&
     &carcri,sdimpr,sddisc,sdpost,sdcrit,sdtime,sderro,sddyna,sdpilo,&
     &sdener,sdieto,sdcriq,lisch2)
            character(len=8) :: result
            integer :: numins
            character(len=24) :: modele
            character(len=24) :: mate
            character(len=24) :: carele
            integer :: fonact(*)
            character(len=24) :: carcri
            character(len=24) :: sdimpr
            character(len=19) :: sddisc
            character(len=19) :: sdpost
            character(len=19) :: sdcrit
            character(len=24) :: sdtime
            character(len=24) :: sderro
            character(len=19) :: sddyna
            character(len=19) :: sdpilo
            character(len=19) :: sdener
            character(len=24) :: sdieto
            character(len=24) :: sdcriq
            character(len=19) :: lisch2
          end subroutine nmarch
        end interface
