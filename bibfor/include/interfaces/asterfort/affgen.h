        interface
          subroutine affgen(tmp,nom,nel,ntel,napcis,foncis)
            character(len=24) :: tmp
            character(len=24) :: nom
            integer :: nel
            integer :: ntel(*)
            character(len=19) :: napcis
            character(len=19) :: foncis
          end subroutine affgen
        end interface
