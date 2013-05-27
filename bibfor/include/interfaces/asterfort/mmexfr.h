        interface
          subroutine mmexfr(noma,defico,izone,posmam,tau1,tau2)
            character(len=8) :: noma
            character(len=24) :: defico
            integer :: izone
            integer :: posmam
            real(kind=8) :: tau1(3)
            real(kind=8) :: tau2(3)
          end subroutine mmexfr
        end interface
