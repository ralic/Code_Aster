        interface
          function dmadt(rho22,sat,phi,mamolg,dp21t,kh,alpha0,biot)
            real(kind=8) :: rho22
            real(kind=8) :: sat
            real(kind=8) :: phi
            real(kind=8) :: mamolg
            real(kind=8) :: dp21t
            real(kind=8) :: kh
            real(kind=8) :: alpha0
            real(kind=8) :: biot
            real(kind=8) :: dmadt
          end function dmadt
        end interface
