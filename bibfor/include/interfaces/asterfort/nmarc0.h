        interface
          subroutine nmarc0(result,modele,mate,carele,fonact,sdcrit,&
     &sddyna,sdpost,carcri,sdcriq,sdpilo,lisch2,numarc,instan)
            character(len=8) :: result
            character(len=24) :: modele
            character(len=24) :: mate
            character(len=24) :: carele
            integer :: fonact(*)
            character(len=19) :: sdcrit
            character(len=19) :: sddyna
            character(len=19) :: sdpost
            character(len=24) :: carcri
            character(len=24) :: sdcriq
            character(len=19) :: sdpilo
            character(len=19) :: lisch2
            integer :: numarc
            real(kind=8) :: instan
          end subroutine nmarc0
        end interface
