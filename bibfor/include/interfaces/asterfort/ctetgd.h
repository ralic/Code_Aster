        interface
          subroutine ctetgd(basmod,numd,numg,nbsec,teta,nbtet)
            integer :: nbtet
            character(len=8) :: basmod
            integer :: numd
            integer :: numg
            integer :: nbsec
            real(kind=8) :: teta(nbtet,nbtet)
          end subroutine ctetgd
        end interface
