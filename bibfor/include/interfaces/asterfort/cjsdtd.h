        interface
          subroutine cjsdtd(mod,q,dtddq)
            character(len=8) :: mod
            real(kind=8) :: q(6)
            real(kind=8) :: dtddq(6,6)
          end subroutine cjsdtd
        end interface
