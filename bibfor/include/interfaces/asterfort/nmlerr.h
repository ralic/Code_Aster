        interface
          subroutine nmlerr(sddisc,action,infz,valr,vali)
            character(len=19) :: sddisc
            character(len=1) :: action
            character(*) :: infz
            real(kind=8) :: valr
            integer :: vali
          end subroutine nmlerr
        end interface
