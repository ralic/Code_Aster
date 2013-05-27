        interface
          subroutine nmextk(noma,motfac,iocc,champ,nomcha,nomchs,&
     &typcha,listno,listma,listpi,listsp,nbno,nbma,nbpi,nbspi,listcp,&
     &nbcmp)
            character(len=8) :: noma
            character(len=16) :: motfac
            integer :: iocc
            character(len=19) :: champ
            character(len=24) :: nomcha
            character(len=24) :: nomchs
            character(len=4) :: typcha
            character(len=24) :: listno
            character(len=24) :: listma
            character(len=24) :: listpi
            character(len=24) :: listsp
            integer :: nbno
            integer :: nbma
            integer :: nbpi
            integer :: nbspi
            character(len=24) :: listcp
            integer :: nbcmp
          end subroutine nmextk
        end interface
