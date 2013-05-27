        interface
          subroutine jacbci(np3,rc,vloc,xloc,kn,cn,ic,jacobc,jacobk)
            integer :: np3
            real(kind=8) :: rc(np3,*)
            real(kind=8) :: vloc(*)
            real(kind=8) :: xloc(*)
            real(kind=8) :: kn
            real(kind=8) :: cn
            integer :: ic
            real(kind=8) :: jacobc(3,*)
            real(kind=8) :: jacobk(3,*)
          end subroutine jacbci
        end interface
