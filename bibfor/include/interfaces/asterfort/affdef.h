        interface
          subroutine affdef(tmp,nom,nel,ntel,tab,ier)
            character(len=24) :: tmp
            character(len=24) :: nom
            integer :: nel
            integer :: ntel(*)
            character(len=8) :: tab(*)
            integer :: ier
          end subroutine affdef
        end interface
