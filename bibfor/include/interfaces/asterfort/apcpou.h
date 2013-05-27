        interface
          subroutine apcpou(sdappa,izone,nommai,typzon,tau1,tau2)
            character(len=19) :: sdappa
            integer :: izone
            character(len=8) :: nommai
            character(len=4) :: typzon
            real(kind=8) :: tau1(3)
            real(kind=8) :: tau2(3)
          end subroutine apcpou
        end interface
