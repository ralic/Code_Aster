        interface
          subroutine nxnoli(modele,mate,carele,lostat,lreuse,lnonl,&
     &levol,para,sddisc,sdcrit,sdieto,lisch2)
            character(len=24) :: modele
            character(len=24) :: mate
            character(len=24) :: carele
            logical :: lostat
            logical :: lreuse
            logical :: lnonl
            logical :: levol
            real(kind=8) :: para(*)
            character(len=19) :: sddisc
            character(len=19) :: sdcrit
            character(len=24) :: sdieto
            character(len=19) :: lisch2
          end subroutine nxnoli
        end interface
