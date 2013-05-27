        interface
          subroutine nmlere(sddisc,action,infz,iterat,valr)
            character(len=19) :: sddisc
            character(len=1) :: action
            character(*) :: infz
            integer :: iterat
            real(kind=8) :: valr(*)
          end subroutine nmlere
        end interface
