        interface
          subroutine gmeelt(imod,nbtyma,nomail,nbnoma,nuconn,nbmail)
            integer :: imod
            integer :: nbtyma
            character(len=8) :: nomail(*)
            integer :: nbnoma(19)
            integer :: nuconn(19,32)
            integer :: nbmail
          end subroutine gmeelt
        end interface
