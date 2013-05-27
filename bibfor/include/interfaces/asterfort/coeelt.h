        interface
          subroutine coeelt(imod,nbtyma,nomail,nbnoma,nuconn,nbmail)
            integer :: imod
            integer :: nbtyma
            character(len=8) :: nomail(*)
            integer :: nbnoma(15)
            integer :: nuconn(15,32)
            integer :: nbmail
          end subroutine coeelt
        end interface
