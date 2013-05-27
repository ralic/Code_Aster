        interface
          subroutine nmdcrg(depart,iterat,vresi,xa0,xa1,xdet)
            integer :: depart
            integer :: iterat
            real(kind=8) :: vresi(*)
            real(kind=8) :: xa0
            real(kind=8) :: xa1
            real(kind=8) :: xdet
          end subroutine nmdcrg
        end interface
