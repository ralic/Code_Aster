        interface
          function dmadp1(rho22,sat,dsatp1,biot,phi,cs,mamolg,kh,&
     &dp21p1,emmag,em)
            real(kind=8) :: rho22
            real(kind=8) :: sat
            real(kind=8) :: dsatp1
            real(kind=8) :: biot
            real(kind=8) :: phi
            real(kind=8) :: cs
            real(kind=8) :: mamolg
            real(kind=8) :: kh
            real(kind=8) :: dp21p1
            logical :: emmag
            real(kind=8) :: em
            real(kind=8) :: dmadp1
          end function dmadp1
        end interface
