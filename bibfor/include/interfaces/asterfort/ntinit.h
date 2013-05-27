        interface
          subroutine ntinit(result,modele,mate,carele,lischa,lisch2,&
     &solveu,para,numedd,lostat,levol,lnonl,sddisc,sdieto,mailla,sdcrit,&
     &time)
            character(len=24) :: result
            character(len=24) :: modele
            character(len=24) :: mate
            character(len=24) :: carele
            character(len=19) :: lischa
            character(len=19) :: lisch2
            character(len=19) :: solveu
            real(kind=8) :: para(*)
            character(len=24) :: numedd
            logical :: lostat
            logical :: levol
            logical :: lnonl
            character(len=19) :: sddisc
            character(len=24) :: sdieto
            character(len=8) :: mailla
            character(len=19) :: sdcrit
            character(len=24) :: time
          end subroutine ntinit
        end interface
