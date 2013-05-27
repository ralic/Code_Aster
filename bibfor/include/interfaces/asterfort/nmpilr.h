        interface
          subroutine nmpilr(fonact,numedd,matass,veasse,residu,eta)
            integer :: fonact(*)
            character(len=24) :: numedd
            character(len=19) :: matass
            character(len=19) :: veasse(*)
            real(kind=8) :: residu
            real(kind=8) :: eta
          end subroutine nmpilr
        end interface
