        interface
          subroutine accep1(modmec,ligrmo,nbm,dir,yang)
            character(len=8) :: modmec
            character(len=24) :: ligrmo
            integer :: nbm
            real(kind=8) :: dir(3,3)
            logical :: yang
          end subroutine accep1
        end interface
