        interface
          subroutine nmpipe(modele,ligrpi,cartyp,careta,mate,compor,&
     &resoco,valinc,depdel,ddepl0,ddepl1,tau,nbeffe,eta,pilcvg,typpil,&
     &carele)
            character(len=24) :: modele
            character(len=19) :: ligrpi
            character(len=19) :: cartyp
            character(len=19) :: careta
            character(len=24) :: mate
            character(len=24) :: compor
            character(len=24) :: resoco
            character(len=19) :: valinc(*)
            character(len=19) :: depdel
            character(len=19) :: ddepl0
            character(len=19) :: ddepl1
            real(kind=8) :: tau
            integer :: nbeffe
            real(kind=8) :: eta(2)
            integer :: pilcvg
            character(len=24) :: typpil
            character(len=24) :: carele
          end subroutine nmpipe
        end interface
