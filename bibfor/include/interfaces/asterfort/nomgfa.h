        interface
          subroutine nomgfa(nogr,nbgr,dgf,nogrf,nbgf)
            integer :: nbgr
            character(len=24) :: nogr(nbgr)
            integer :: dgf(*)
            character(len=80) :: nogrf(*)
            integer :: nbgf
          end subroutine nomgfa
        end interface
