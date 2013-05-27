        interface
          subroutine tiinit(mailla,modele,resulz,lostat,lreuse,lnonl,&
     &instin,sddisc,sdieto,sdobse,levol)
            character(len=8) :: mailla
            character(len=24) :: modele
            character(len=24) :: resulz
            logical :: lostat
            logical :: lreuse
            logical :: lnonl
            real(kind=8) :: instin
            character(len=19) :: sddisc
            character(len=24) :: sdieto
            character(len=19) :: sdobse
            logical :: levol
          end subroutine tiinit
        end interface
