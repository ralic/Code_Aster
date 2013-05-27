        interface
          subroutine copnor(noma,defico,resoco,posmai,ksi1,ksi2,tau1,&
     &tau2)
            character(len=8) :: noma
            character(len=24) :: defico
            character(len=24) :: resoco
            integer :: posmai
            real(kind=8) :: ksi1
            real(kind=8) :: ksi2
            real(kind=8) :: tau1(3)
            real(kind=8) :: tau2(3)
          end subroutine copnor
        end interface
