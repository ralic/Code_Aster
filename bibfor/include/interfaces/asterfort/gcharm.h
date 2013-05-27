        interface
          subroutine gcharm(fonc,charg,nomfon,nomf,time,iord,chargm)
            logical :: fonc
            character(len=19) :: charg
            character(len=24) :: nomfon
            character(len=8) :: nomf
            real(kind=8) :: time
            integer :: iord
            character(len=19) :: chargm
          end subroutine gcharm
        end interface
