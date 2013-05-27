        interface
          subroutine nmceni(numedd,depdel,deppr1,deppr2,rho,sdpilo,eta&
     &,isxfe,f)
            character(len=24) :: numedd
            character(len=19) :: depdel
            character(len=19) :: deppr1
            character(len=19) :: deppr2
            real(kind=8) :: rho
            character(len=19) :: sdpilo
            real(kind=8) :: eta
            logical :: isxfe
            real(kind=8) :: f
          end subroutine nmceni
        end interface
