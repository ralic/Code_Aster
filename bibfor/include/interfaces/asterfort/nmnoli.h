        interface
          subroutine nmnoli(result,sddisc,sderro,carcri,sdimpr,sdcrit,&
     &fonact,sddyna,sdpost,modele,mate,carele,lisch2,sdpilo,sdtime,&
     &sdener,sdieto,sdcriq)
            character(len=8) :: result
            character(len=19) :: sddisc
            character(len=24) :: sderro
            character(len=24) :: carcri
            character(len=24) :: sdimpr
            character(len=19) :: sdcrit
            integer :: fonact(*)
            character(len=19) :: sddyna
            character(len=19) :: sdpost
            character(len=24) :: modele
            character(len=24) :: mate
            character(len=24) :: carele
            character(len=19) :: lisch2
            character(len=19) :: sdpilo
            character(len=24) :: sdtime
            character(len=19) :: sdener
            character(len=24) :: sdieto
            character(len=24) :: sdcriq
          end subroutine nmnoli
        end interface
