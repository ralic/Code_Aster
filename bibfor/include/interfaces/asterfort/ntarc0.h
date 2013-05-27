        interface
          subroutine ntarc0(result,modele,mate,carele,sdcrit,lisch2,&
     &lnonl,para,numarc,instan)
            character(len=8) :: result
            character(len=24) :: modele
            character(len=24) :: mate
            character(len=24) :: carele
            character(len=19) :: sdcrit
            character(len=19) :: lisch2
            logical :: lnonl
            real(kind=8) :: para(*)
            integer :: numarc
            real(kind=8) :: instan
          end subroutine ntarc0
        end interface
