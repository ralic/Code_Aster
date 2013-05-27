        interface
          subroutine jacbpy(kn,cn,jacobc,jacobk)
            real(kind=8) :: kn
            real(kind=8) :: cn
            real(kind=8) :: jacobc(3,*)
            real(kind=8) :: jacobk(3,*)
          end subroutine jacbpy
        end interface
