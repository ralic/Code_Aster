        interface
          subroutine ntarch(numins,modele,mate,carele,lnonl,para,&
     &sddisc,sdcrit,sdieto,lisch2,force)
            integer :: numins
            character(len=24) :: modele
            character(len=24) :: mate
            character(len=24) :: carele
            logical :: lnonl
            real(kind=8) :: para(*)
            character(len=19) :: sddisc
            character(len=19) :: sdcrit
            character(len=24) :: sdieto
            character(len=19) :: lisch2
            logical :: force
          end subroutine ntarch
        end interface
