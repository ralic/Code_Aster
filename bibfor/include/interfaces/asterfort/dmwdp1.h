        interface
          function dmwdp1(rho11,signe,sat,dsatp1,biot,phi,cs,cliq,&
     &dp11p1,emmag,em)
            real(kind=8) :: rho11
            real(kind=8) :: signe
            real(kind=8) :: sat
            real(kind=8) :: dsatp1
            real(kind=8) :: biot
            real(kind=8) :: phi
            real(kind=8) :: cs
            real(kind=8) :: cliq
            real(kind=8) :: dp11p1
            logical :: emmag
            real(kind=8) :: em
            real(kind=8) :: dmwdp1
          end function dmwdp1
        end interface
